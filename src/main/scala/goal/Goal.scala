package goal

/**
  * Created by matt on 29/12/2015.
  */
trait Goal {

  def isSatisfied(state: State): Boolean

}