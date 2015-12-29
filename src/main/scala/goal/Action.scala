package goal

import scala.concurrent.Future

trait SerializedAction[I, O] {

  def deserialize: Action[I, O]

}

/**
  * Created by matt on 29/12/2015.
  */
trait Action[I, O] extends (I => Future[O]) {

  def serialize: SerializedAction[I, O]

}
