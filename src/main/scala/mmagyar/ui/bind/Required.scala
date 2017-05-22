package mmagyar.ui.bind


/** Created by Magyar Máté on 2017-05-22, All rights reserved. */
trait Required[T,K <: Required[T,K]] extends Bind {this:K =>
  def transform(value:T):K
}
