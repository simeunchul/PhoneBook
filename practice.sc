import PhoneNumberBook.Member

import scala.util.matching.Regex
private var Members = List[Member]()

val member = Member("name","phone",Option("add"))
val memberOption = Option(member)
memberOption.flatMap(member => member.address)
val member2 = Member("name","phone",None)
val memberOption2 = Option(member2)
memberOption2.flatMap(member => member.address)
memberOption2.map(member => member.address)
  .flatMap(x => x)
memberOption.map(member => member.address)
  .flatMap(x => x).isDefined
val inputName ="abc"

val hangeulPattern: Regex = "[^가-힣]".r


Some(10).map(x => x)
List(Some(12), None, Some(90)).flatMap(x => x)
Some("hi").map(x => x)
List(Some(12), None, Some(90)).map(x=>x)
Option("hi").map(x => x)
List(12,30,40,None).flatMap(x => Option(x))

val o: Option[Int] = Some(1)
println(o)
o.map(o => println(o))

val x = Option(5)
val y = x.map(x => println(x + 10))
y
val z = x.map(_ + 10)
z
println(z)
val k = None
k
val j =  k.map(x => x)

