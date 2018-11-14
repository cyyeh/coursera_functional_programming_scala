object combinatariolSearch2 {
  val fruit = Set("apple", "banana", "pear")
  val s = (1 to 6).toSet

  s map (_ + 2)
  fruit filter (_.startsWith("app"))
  s.nonEmpty
}