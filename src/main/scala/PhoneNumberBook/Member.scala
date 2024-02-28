package PhoneNumberBook

case class Member(name: String, phoneNumber: String, address: Option[String]){

  private val strAddresss = address match {
    case Some(strAddress) => strAddress
    case None =>
  }
  override def toString: String = {

    s"name : $name  phoneNumber : $phoneNumber  address : $strAddresss"
  }
}
