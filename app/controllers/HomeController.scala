package controllers

import play.api.mvc._

import scala.concurrent.{ExecutionContext}

/**
 * The controller for the index pages.
 */
class HomeController(cc: ControllerComponents)(implicit ec: ExecutionContext)  extends AbstractController(cc) {
  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def healthcheck() = Action { implicit request: Request[AnyContent] =>
    Ok("""{ "healthy" : "true" }""")
  }
}
