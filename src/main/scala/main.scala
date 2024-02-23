import scala.util.matching.Regex
import scala.io.StdIn.readLine
object main {

  private val hangeulPattern: Regex = "[^가-힣]".r
  private val numberPattern: Regex = "[^0-9]".r
  private var Members = List[Member]()
  private case class Member(name: String, phoneNumber: String, address:Option[String])
  //시작화면
  def showMenu(): Unit = {
    println(
      """
        |전화번호부
        |1.등록
        |2.찾기
        |3.삭제
        |4.전체목록
        |5.수정
        |6.종료
        |""".stripMargin
    )
    readLine() match {
      case "1" => register()
      case "2" => rudMenu("search")
      case "3" => println("삭제 대상 찾기")
        rudMenu("delete")
      case "4" => list()
      case "5" => println("수정 대상 찾기")
        rudMenu("update")
      case "6" => System.exit(0);
      case _ => println("입력오류")
        println("1~6 사이의 숫자를 입력해주세요")
        showMenu();
    }
  }
  //read,update,delete menu
  def rudMenu(rudKey: String): Unit = {
    //readMap
    val searchMap = Map[String, Unit](("name",input("이름입력",searchName)),("phoneNumber",input("번호입력",searchPhoneNumber)),("address",input("주소입력",searchAddress)))
    //updateMap
    val updateMap = Map[String, Unit](("name",input("이름입력",updateName)),("phoneNumber",input("번호입력",updatePhoneNumber)),("address",input("주소입력",updateAddress)))
    //deleteMap
    val deleteMap = Map[String, Unit](("name",input("이름입력",deleteName)),("phoneNumber",input("번호입력",deletePhoneNumber)),("address",input("주소입력",deleteAddress)))
    //rud ==> read update delete
    val rudMap = Map("search" -> searchMap)
    println(
      """
        |1.이름으로 찾기
        |2.전화번호로 찾기
        |3.주소로 찾기
        |""".stripMargin
    )
    readLine() match {
      case "1" => rudMap(rudKey)("name")
      case "2" => rudMap(rudKey)("phoneNumber")
      case "3" => rudMap(rudKey)("address")
    }
  }

  //시작화면 끝

  //인풋하는 함수 , 밸리데이트는 확인하는 함수 input(message, validate) 형태
  //  //입력을 받는 형태에따라 >> message로 입력요구 그에 맞는 validate
  //  //name validate , phone validate 두개 만들어아햠
  //  //input("이름입력",namevalidate) 이런식이 될것임

  def input(message:String , validate :String => Option[String]):Option[String] = {
    println(message)
    val inputValue = readLine()
    validate(inputValue)
  }

  def input(message: String, validate: String => Unit): Unit = {
    println(message)
    val inputValue = readLine()
    validate(inputValue)
  }

  //1.등록
  def register(): Unit = {
    val inputName = input("이름입력",validateName).getOrElse("Nothing")
    val inputPhone = input("번호입력",validatePhone).getOrElse("Nothing")
    println("주소입력")
    val inputAddress = Option(readLine())
    Members = Members :+ Member(inputName,inputPhone,inputAddress)
    println("등록완료")
    println(Members)
    showMenu()
  }


  def validateName(inputName:String): Option[String] = { // 이름 한글검사
    hangeulPattern.findFirstMatchIn(inputName) match {
      case None => println("한글확인")
      val nameOption = Option(inputName)
      nameOption
      case Some(_) => println("한글만 입력해주세요")
        input("이름입력",validateName)
      case _ => println("오류")
        input("이름입력",validateName)
    }
  }

  def validatePhone(inputPhoneNumber: String): Option[String] = { //전화번호 숫자검사
    numberPattern.findFirstMatchIn(inputPhoneNumber) match {
      case None => println("숫자확인")
        duplicatePhone(inputPhoneNumber)
      case Some(_) => println("숫자만 입력해주세요")
        input("번호입력",validatePhone)
      case _ => println("오류")
        input("번호입력",validatePhone)
    }
  }
  def duplicatePhone(inputPhoneNumber:String) :Option[String] ={
    val inputPhoneNumberOption = Option(inputPhoneNumber)
    val duplicateCheckMember = Members.find(member => member.phoneNumber==inputPhoneNumber)
    if(duplicateCheckMember.isEmpty) inputPhoneNumberOption
    else{
       val duplicateCheckPhoneNumber = duplicateCheckMember.get.phoneNumber
       inputPhoneNumber match {
          case `duplicateCheckPhoneNumber` => println("중복된 번호가 있습니다. 다시 입력해주세요")
            input("번호입력",validatePhone)
          case _ => inputPhoneNumberOption
        }
    }
  }
  //등록 끝

  /*
  def searchMenu():Unit = {
    println(
      """
        |1.이름으로 찾기
        |2.전화번호로 찾기
        |3.주소로 찾기
        |""".stripMargin
    )
    readLine() match {
      case "1" => search("이름 입력", searchName)
      case "2" => search("전화번호 입력", searchPhoneNumber)
      case "3" => search("주소 입력", searchAddress)
    }
  }
  def search(message:String, search: String=> Unit): Unit = {
    println(message)
    val input = readLine()
    search(input)
  }*/

  def searchName(inputName: String):Unit = {
    val findMemberOption = Members.find(member => member.name == inputName)
    if(findMemberOption.isEmpty) {
      println("찾는 정보가 없습니다.")
      showMenu()
    }
    else {
      println("찾기완료" + findMemberOption.get)
      showMenu()
    }
  }

  def searchPhoneNumber(inputPhoneNumber: String):Unit = {
    val findMemberOption = Members.find(member => member.phoneNumber == inputPhoneNumber)
    if(findMemberOption.isEmpty) {
      println("찾는 정보가 없습니다.")
      showMenu()
    }
    else {
      println("찾기완료" + findMemberOption.get)
      showMenu()

    }
  }

  def searchAddress(inputAddress: String): Unit = {
    val findMemberOption = Members.find(member => member.address.get == inputAddress)
    if(findMemberOption.isEmpty) {
      println("찾는 정보가 없습니다.")
      showMenu()
    }
    else {
      println("찾기완료" + findMemberOption.get)
      showMenu()
    }
  }
  /* 변수만 다르고 반복되는걸 한개로 합치는 방법은 없을까
  def SearchProcess(input:String):Unit = {
    val findMemberOption = Members.find(Member => Member.address.get == input)
    if(findMemberOption.isEmpty) {println("찾는 정보가 없습니다.")
      ShowMenu()
    }
    else {
      println("찾기완료" + findMemberOption.get)
      ShowMenu()
    }
  }*/

  //3. 삭제
  /*
  def deleteMenu(): Unit = {
    println(
      """
        |1.이름으로 찾기
        |2.전화번호로 찾기
        |3.주소로 찾기
        |""".stripMargin
    )
    readLine() match {
      case "1" => delete("이름 입력", deleteName)
      case "2" => delete("전화번호 입력", deletePhoneNumber)
      case "3" => delete("주소 입력", deleteAddress)
    }
  }



  def delete(message:String, delete: String=> Unit): Unit = {
    println(message)
    val input = readLine()
    delete(input)
  }

   */
  def deleteName(inputName: String): Unit = {
    val findMemberOption = Members.find(member => member.name == inputName)
    if(findMemberOption.isEmpty) {
      println("찾는 정보가 없습니다.")
      showMenu()
    }
    else {
      Members = Members.filterNot(member => member.name == inputName)
      println("삭제완료")
      showMenu()
    }
  }

  def deletePhoneNumber(inputPhoneNumber: String): Unit = {
    val findMemberOption = Members.find(member => member.phoneNumber == inputPhoneNumber)
    if(findMemberOption.isEmpty) {
      println("찾는 정보가 없습니다.")
      showMenu()
    }
    else {
      Members = Members.filterNot(member => member.phoneNumber == inputPhoneNumber)
      println("삭제완료")
      showMenu()
    }
  }

  def deleteAddress(inputAddress: String): Unit = {
    val findMemberOption = Members.find(member => member.address.get == inputAddress)
    if(findMemberOption.isEmpty) {
      println("찾는 정보가 없습니다.")
      showMenu()
    }
    else {
      Members = Members.filterNot(member => member.address.get == inputAddress)
      println("삭제완료")
      showMenu()
    }
  }

  //5. 수정
  /*
  def updateMenu(): Unit = {
    println(
      """
        |1.이름으로 찾기
        |2.전화번호로 찾기
        |3.주소로 찾기
        |""".stripMargin
    )
    readLine() match {
      case "1" => update("이름 입력", updateName)
      case "2" => update("전화번호 입력", updatePhoneNumber)
      case "3" => update("주소 입력", updateAddress)
    }
  }
  def update(message:String, update: String=> Unit): Unit = {
    println(message)
    val input = readLine()
    update(input)
  }

   */
  //옵션 겟 추후에 처리해야됨!!!!!
  def updateName(inputName: String): Unit = {
    val updateMemberOption = Members.find(member => member.name == inputName)
    if(updateMemberOption.isEmpty){
      println("찾는 정보가 없습니다.")
      showMenu()
    }
    else {
      val inputName = input("수정할 이름입력", validateName).getOrElse("Nothing")
      val inputPhone = input("수정할 번호입력", updateValidatePhone).getOrElse("Nothing")
      println("수정할 주소입력")
      val inputAddress = Option(scala.io.StdIn.readLine())
      Members = Members.updated(Members.indexOf(updateMemberOption.get),Member(inputName,inputPhone,inputAddress))
      println("수정완료")
      showMenu()
    }
  }
  def updatePhoneNumber(inputPhoneNumber: String): Unit = {
    val updateMemberOption = Members.find(member => member.phoneNumber == inputPhoneNumber)
    if(updateMemberOption.isEmpty){
      println("찾는 정보가 없습니다.")
      showMenu()
    }
    else {
      val inputName = input("수정할 이름입력", validateName).getOrElse("Nothing")
      val inputPhone = input("수정할 번호입력", updateValidatePhone).getOrElse("Nothing")
      println("수정할 주소입력")
      val inputAddress = Option(scala.io.StdIn.readLine())
      Members = Members.updated(Members.indexOf(updateMemberOption.get),Member(inputName,inputPhone,inputAddress))
      println("수정완료")
      showMenu()
    }
  }
  def updateAddress(inputAddress:String):Unit = {
    val updateMemberOption = Members.find(member => member.address.get == inputAddress)
    if(updateMemberOption.isEmpty){
      println("찾는 정보가 없습니다.")
      showMenu()
    }
    else {
      val inputName = input("수정할 이름입력", validateName).getOrElse("Nothing")
      val inputPhone = input("수정할 번호입력", validatePhone).getOrElse("Nothing")
      println("수정할 주소입력")
      val inputAddress = Option(scala.io.StdIn.readLine())
      Members = Members.updated(Members.indexOf(updateMemberOption.get),Member(inputName,inputPhone,inputAddress))
      println("수정완료")
      showMenu()
    }
  }
  def updateValidatePhone(inputPhoneNumber: String): Option[String] = { //전화번호 숫자검사
    numberPattern.findFirstMatchIn(inputPhoneNumber) match {
      case None => println("숫자확인")
        Option(inputPhoneNumber)
      case Some(_) => println("숫자만 입력해주세요")
        input("번호입력",validatePhone)
      case _ => println("오류")
        input("번호입력",validatePhone)
    }
  }

  //수정 끝

  //5.전체목록
  def list(): Unit = {
    println("전체목록")
    Members.foreach(member => println(member))
    println("")
    showMenu()

  }
  //전체목록 끝





  def main(args: Array[String]): Unit = {
    showMenu()
  }



}
