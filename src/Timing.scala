object Timer {
  def time(f: => Unit)={
    val s = System.currentTimeMillis
    f
    (System.currentTimeMillis - s)/1000
  }
}
