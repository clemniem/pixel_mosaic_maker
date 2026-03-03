package clemniem

import cats.effect.IO
import tyrian.{Cmd, Html, Sub}

/** Root-level messages: navigation or delegation to the current screen.
  * The `output` in [[NavigateTo]] is `Any` so the framework has no dependency on application-level output types.
  * Each screen's `init` casts it to the expected type via pattern matching.
  */
sealed trait RootMsg
object RootMsg {
  case class NavigateTo(screenId: ScreenId, output: Option[Any]) extends RootMsg
  case class HandleScreenMsg(screenId: ScreenId, msg: Any)       extends RootMsg
}

/** Message a screen can emit to request navigation to another screen with optional output data.
  * Intercepted in [[Screen.wrapMsg]] and converted to [[RootMsg.NavigateTo]].
  */
case class NavigateNext(screenId: ScreenId, output: Option[Any])

/** Abstraction for a single screen in the SPA. Each screen has its own Model, Msg, init, update, view, and
  * subscriptions. Use [[wrapMsg]] to lift screen messages to [[RootMsg]] so the root app can run [[Cmd]]/[[Sub]].
  *
  * The framework is decoupled from application-level output types: `init` receives `Option[Any]` and each screen
  * pattern-matches on the expected type. Use [[navCmd]] / [[navMsg]] helpers to navigate between screens.
  */
trait Screen {
  type Model
  type Msg

  /** Unique identifier for this screen (use this, not `id`, to avoid shadowing HTML's `id` attribute). */
  val screenId: ScreenId

  /** Initial state and optional command. `previous` is the navigation output from the previous screen (if any).
    * Cast to the expected application-level output type via pattern matching.
    */
  def init(previous: Option[Any]): (Model, Cmd[IO, Msg])

  /** Update state and optional command in response to a message. */
  def update(model: Model): Msg => (Model, Cmd[IO, Msg])

  /** View for this screen. Root app will map [[Msg]] to [[RootMsg]] via [[wrapMsg]]. */
  def view(model: Model): Html[Msg]

  /** Optional subscriptions. Default: none. */
  def subscriptions(model: Model): Sub[IO, Msg] = Sub.None

  /** Lifts a screen message to [[RootMsg]] so the root app can dispatch it.
    * [[NavigateNext]] is intercepted and converted to [[RootMsg.NavigateTo]] so the root handler never needs
    * to dig into `Any` to find navigation requests.
    */
  def wrapMsg(msg: Msg): RootMsg = (msg: Any) match {
    case n: NavigateNext => RootMsg.NavigateTo(n.screenId, n.output)
    case _               => RootMsg.HandleScreenMsg(screenId, msg)
  }

  /** Create a [[Cmd]] that navigates to another screen. The emitted [[NavigateNext]] is converted to
    * [[RootMsg.NavigateTo]] by [[wrapMsg]], so it never reaches any screen's update function.
    */
  protected def navCmd(target: ScreenId, output: Option[Any]): Cmd[IO, Msg] =
    Cmd.Emit(NavigateNext(target, output).asInstanceOf[Msg])

  /** Create a [[NavigateNext]] typed as this screen's [[Msg]]. For use in `onClick` handlers and callbacks.
    * [[wrapMsg]] will convert it to [[RootMsg.NavigateTo]].
    */
  protected def navMsg(target: ScreenId, output: Option[Any]): Msg =
    NavigateNext(target, output).asInstanceOf[Msg]
}

/** Type-safe wrapper that pairs a [[Screen]] with its model, encapsulating the `asInstanceOf` casts in one place.
  * The rest of the application (root update, view, subscriptions) works with `ActiveScreen` and never sees `Any`.
  *
  * The message cast is the only remaining unsafe boundary — under JVM/JS type erasure the screen-level Msg type
  * cannot be checked at runtime. The guard `screenId == model.currentScreenId` in the root update prevents stale
  * messages from reaching the wrong screen.
  */
final class ActiveScreen private (val screen: Screen, private val modelValue: Any) {

  def view: Html[RootMsg] =
    screen.view(modelValue.asInstanceOf[screen.Model]).map(screen.wrapMsg)

  def subscriptions: Sub[IO, RootMsg] =
    screen.subscriptions(modelValue.asInstanceOf[screen.Model]).map(screen.wrapMsg)

  def update(msg: Any): (ActiveScreen, Cmd[IO, RootMsg]) = {
    val (newModel, cmd) =
      screen.update(modelValue.asInstanceOf[screen.Model])(msg.asInstanceOf[screen.Msg])
    (new ActiveScreen(screen, newModel), cmd.map(screen.wrapMsg))
  }
}

object ActiveScreen {

  /** Create an [[ActiveScreen]] by running a screen's `init`. */
  def fromInit(screen: Screen, output: Option[Any]): (ActiveScreen, Cmd[IO, RootMsg]) = {
    val (model, cmd) = screen.init(output)
    (new ActiveScreen(screen, model), cmd.map(screen.wrapMsg))
  }
}
