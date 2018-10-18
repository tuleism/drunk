/*
 * Copyright 2018 Facundo Viale
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.jarlakxen.drunk

import scala.concurrent.{ExecutionContext, Future}
import scala.util._
import com.softwaremill.sttp._
import com.softwaremill.sttp.circe._
import extensions.{GraphQLExtensions, NoExtensions}
import io.circe._
import io.circe.parser._
import sangria.ast.Document
import sangria.introspection._
import sangria.marshalling.circe._
import sangria.parser.{QueryParser}

class GraphQLClient private[GraphQLClient] (uri: Uri, options: ClientOptions, headers: Seq[(String, String)])(implicit backend: SttpBackend[Future, Nothing]) {
  import GraphQLClient._

  private[drunk] def execute[Res, Vars](doc: Document, variables: Option[Vars], name: Option[String])(
    implicit
    variablesEncoder: Encoder[Vars],
    ec: ExecutionContext
  ): Future[(Int, Json)] =
    execute(GraphQLOperation(doc, variables, name))

  private def buildRequest(body: String) = sttp
    .post(uri)
    .body(body)
    .response(asString)
    .headers(headers: _*)

  private[drunk] def execute[Res, Vars](op: GraphQLOperation[Res, Vars])(
    implicit
    ec: ExecutionContext
  ): Future[(Int, Json)] = {

    val fields =
      List("query" -> op.docToJson) ++
        op.encodeVariables.map("variables" -> _) ++
        op.name.map("operationName" -> Json.fromString(_))

    val body: String = Json.obj(fields: _*).noSpaces
    val request = buildRequest(body)

    for {
      (statusCode, rawBody) <- request.send.map {
        case Response(rawErrorBody, code, _, _, _) =>
          val body = rawErrorBody
            .left
            .map(new String(_))
            .merge

          (code, body)
      }
      jsonBody <- Future.fromTry(parse(rawBody).toTry)
    } yield (statusCode, jsonBody)
  }

  def query[Res](doc: String)(implicit dec: Decoder[Res], ec: ExecutionContext): Try[GraphQLCursor[Res, Nothing]] =
    query(doc, None, None)(dec, null, ec)

  def query[Res](
    doc: String,
    operationName: String
  )(implicit dec: Decoder[Res], ec: ExecutionContext): Try[GraphQLCursor[Res, Nothing]] =
    query(doc, None, Some(operationName))(dec, null, ec)

  def query[Res, Vars](doc: String, variables: Vars)(
    implicit
    dec: Decoder[Res],
    en: Encoder[Vars],
    ec: ExecutionContext
  ): Try[GraphQLCursor[Res, Vars]] =
    query(doc, Some(variables), None)

  def query[Res, Vars](doc: String, variables: Option[Vars], operationName: Option[String])(
    implicit
    dec: Decoder[Res],
    en: Encoder[Vars],
    ec: ExecutionContext
  ): Try[GraphQLCursor[Res, Vars]] =
    QueryParser.parse(doc).map(query(_, variables, operationName))

  def query[Res](doc: Document)(implicit dec: Decoder[Res], ec: ExecutionContext): GraphQLCursor[Res, Nothing] =
    query(doc, None, None)(dec, null, ec)

  def query[Res](
    doc: Document,
    operationName: String
  )(implicit dec: Decoder[Res], ec: ExecutionContext): GraphQLCursor[Res, Nothing] =
    query(doc, None, Some(operationName))(dec, null, ec)

  def query[Res, Vars](doc: Document, variables: Vars)(
    implicit
    dec: Decoder[Res],
    en: Encoder[Vars],
    ec: ExecutionContext
  ): GraphQLCursor[Res, Vars] =
    query(doc, Some(variables), None)

  def query[Res, Vars](doc: Document, variables: Option[Vars], operationName: Option[String])(
    implicit
    dec: Decoder[Res],
    en: Encoder[Vars],
    ec: ExecutionContext
  ): GraphQLCursor[Res, Vars] = {
    var fullDoc = doc
    if (options.addTypename) {
      fullDoc = ast.addTypename(doc)
    }

    val operation: GraphQLOperation[Res, Vars] = GraphQLOperation(doc, variables, operationName)
    val result = execute(operation)
    val data: Future[GraphQLClient.GraphQLResponse[Res]] = result.flatMap {
      case (status, body) => Future.fromTry(extractErrorOrData(body, status))
    }
    val extensions = result.map { case (_, body) => extractExtensions(body) }
    new GraphQLCursor(this, data, extensions, operation)
  }

  def mutate[Res, Vars](doc: Document, variables: Vars)(
    implicit
    dec: Decoder[Res],
    en: Encoder[Vars],
    ec: ExecutionContext
  ): Future[GraphQLResponse[Res]] =
    mutate(doc, Some(variables), None)

  def mutate[Res, Vars](doc: Document, variables: Vars, operationName: Option[String])(
    implicit
    dec: Decoder[Res],
    en: Encoder[Vars],
    ec: ExecutionContext
  ): Future[GraphQLResponse[Res]] = {

    val result = execute(doc, Some(variables), operationName)
    result.flatMap { case (status, body) => Future.fromTry(extractErrorOrData(body, status)) }
  }

  def schema(implicit ec: ExecutionContext): Future[IntrospectionSchema] =
    execute[Json, Nothing](introspectionQuery, None, None)(null, ec)
      .flatMap {
        case (_, json) => Future.fromTry(IntrospectionParser.parse(json))
      }

}

object GraphQLClient {

  // Work arround for Scala 2.11
  implicit class Either212[+A, +B](either: Either[A, B]) {

    def toTry(implicit ev: A <:< Throwable): Try[B] = either match {
      case Right(b) => Success(b)
      case Left(a) => Failure(a)
    }

    def toOption: Option[B] = either match {
      case Right(b) => Some(b)
      case _        => None
    }
  }

  type GraphQLResponse[Res] = Either[GraphQLResponseError, GraphQLResponseData[Res]]

  def apply(
    uri: String,
    clientOptions: ClientOptions = ClientOptions.Default,
    headers: Seq[(String, String)] = Seq.empty
  )(implicit backend: SttpBackend[Future, Nothing]): GraphQLClient =
    new GraphQLClient(uri"$uri", clientOptions, headers)(backend)

  private[GraphQLClient] def extractErrors(body: Json, statusCode: Int): Option[GraphQLResponseError] = {
    val cursor: HCursor = body.hcursor

    for {
      errorsNode <- cursor.downField("errors").focus
      errors <- errorsNode.asArray
    } yield {
      val msgs = errors.flatMap(_.hcursor.downField("message").as[String].toOption).toList
      GraphQLResponseError(msgs, statusCode)
    }
  }

  private[GraphQLClient] def extractData[Res](jsonBody: Json)(implicit dec: Decoder[Res]): Try[GraphQLResponseData[Res]] =
    jsonBody.hcursor.downField("data").as[Res].toTry.map(GraphQLResponseData(_))

  private[GraphQLClient] def extractErrorOrData[Res](jsonBody: Json, statusCode: Int)(implicit dec: Decoder[Res]): Try[GraphQLResponse[Res]] = {
    val errors: Option[Try[GraphQLResponse[Res]]] =
      extractErrors(jsonBody, statusCode).map(errors => Success(Left(errors)))
    val data: Try[GraphQLResponse[Res]] =
      extractData(jsonBody).map(Right(_))

    errors.getOrElse(data)
  }

  private[GraphQLClient] def extractExtensions(jsonBody: Json): GraphQLExtensions =
    jsonBody
      .hcursor
      .downField("extensions")
      .as[GraphQLExtensions]
      .toOption
      .getOrElse(NoExtensions)

}
