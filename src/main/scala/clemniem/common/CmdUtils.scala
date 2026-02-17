package clemniem.common

import cats.effect.IO
import tyrian.Cmd

/** Standardized command helpers that wrap IO with error handling via `IO.attempt`.
  *
  * Ported from gbcamutil's CmdUtils pattern. Every IO-based command goes through `attempt` so that exceptions are
  * caught and mapped to a message instead of being swallowed silently.
  */
object CmdUtils {

  /** Run an IO and map the result to a message, with explicit error handling.
    *
    * Wraps `io.attempt` so that any thrown exception is caught and mapped via `onError` instead of being lost. Use this
    * instead of raw `Cmd.Run(io, toMsg)`.
    *
    * @param io
    *   the effect to execute
    * @param toMsg
    *   maps a successful result to a message
    * @param onError
    *   maps a failure to a message (e.g. an error message in the model)
    */
  def run[A, M](io: IO[A], toMsg: A => M, onError: Throwable => M): Cmd[IO, M] =
    Cmd.Run(
      io.attempt,
      {
        case Right(v) => toMsg(v)
        case Left(e)  => onError(e)
      })

  /** Fire-and-forget: run an IO whose result we don't need, but still catch errors.
    *
    * Typically used for draw commands and other side effects where the only feedback we care about is whether it
    * succeeded or not.
    *
    * @param io
    *   the effect to execute (result is discarded on success)
    * @param noOp
    *   the message to emit on success (usually a screen's NoOp)
    * @param onError
    *   maps a failure to a message
    */
  def fireAndForget[M](io: IO[Unit], noOp: M, onError: Throwable => M): Cmd[IO, M] =
    run[Unit, M](io, _ => noOp, onError)
}
