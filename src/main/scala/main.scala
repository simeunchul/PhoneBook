import scala.util.matching.Regex

object main {

  private val HangeulPattern: Regex = "[^가-힣]".r
  private val NumberPattern: Regex = "[^0-9]".r
  private var Members = List[Member]()
  private case class Member(name: String, phoneNumber: String, address:Option[String])

  //시작화면
  def ShowMenu(): Unit = {
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
    val input = scala.io.StdIn.readLine() match {
      case "1" => Register()
      case "2" => SearchMenu()
      case "3" => println("삭제 대상 찾기")
                  DeleteMenu()
      case "4" => ListProcess()
      case "5" => println("수정 대상 찾기")
                  UpdateMenu()
      case "6" => System.exit(0);
      case _ => println("입력오류")
        println("1~6 사이의 숫자를 입력해주세요")
        ShowMenu();
    }
  }

  //시작화면 끝

  //인풋하는 함수 , 밸리데이트는 확인하는 함수 input(message, validate) 형태
  //  //입력을 받는 형태에따라 >> message로 입력요구 그에 맞는 validate
  //  //name validate , phone validate 두개 만들어아햠
  //  //Input("이름입력",namevalidate) 이런식이 될것임

  def Input(message:String , func :String => Option[String]):Option[String] = {
    println(message)
    val inputvalue = scala.io.StdIn.readLine()
    func(inputvalue)
  }

  //1.등록
  def Register(): Unit = {
    val InputName = Input("이름입력",NameValidate).getOrElse("Nothing")
    val InputPhone = Input("번호입력",PhoneValidate).getOrElse("Nothing")
    println("주소입력")
    val InputAddress = Option(scala.io.StdIn.readLine())
    Members = Members :+ Member(InputName,InputPhone,InputAddress)
    println("등록완료")
    println(Members)
    println(Members.last.address.get)
    ShowMenu()
  }


  def NameValidate(InputName:String): Option[String] = { // 이름 한글검사
    HangeulPattern.findFirstMatchIn(InputName) match {
      case None => println("한글확인")
       val NameOption = Option(InputName)
      NameOption
      case Some(_) => println("한글만 입력해주세요")
        Input("이름입력",NameValidate)
      case _ => println("오류")
        Input("이름입력",NameValidate)
    }
  }

  def PhoneValidate(InputPhoneNumber: String): Option[String] = { //전화번호 숫자검사
    NumberPattern.findFirstMatchIn(InputPhoneNumber) match {
      case None => println("숫자확인")
        PhoneDuplicate(InputPhoneNumber)
      case Some(_) => println("숫자만 입력해주세요")
        Input("번호입력",PhoneValidate)
      case _ => println("오류")
        Input("번호입력",PhoneValidate)
    }
  }
  def PhoneDuplicate(InputPhoneNumber:String) :Option[String] ={
    val InputPhoneNumberOption = Option(InputPhoneNumber)
    val DuplicateCheckMember = Members.find(member => (member.phoneNumber==InputPhoneNumber))
    if(DuplicateCheckMember.isEmpty) InputPhoneNumberOption
    else{
       val DuplicateCheckPhoneNumber = DuplicateCheckMember.get.phoneNumber
       InputPhoneNumber match {
          case DuplicateCheckPhoneNumber => println("중복된 번호가 있습니다. 다시 입력해주세요")
            Input("번호입력",PhoneValidate)
          case _ => InputPhoneNumberOption
        }
    }
  }
  //등록 끝


  def SearchMenu():Unit = {
    println(
      """
        |1.이름으로 찾기
        |2.전화번호로 찾기
        |3.주소로 찾기
        |""".stripMargin
    )
    val input = scala.io.StdIn.readLine() match {
      case "1" => Search("이름 입력", NameSearch)
      case "2" => Search("전화번호 입력", PhoneNumberSearch)
      case "3" => Search("주소 입력", AddressSearch)
    }
  }
  def Search(message:String, func: String=> Unit): Unit = {
    println(message)
    val input = scala.io.StdIn.readLine()
    func(input)
  }

  def NameSearch(input: String):Unit = {
    val FindMemberOption = Members.find(member => member.name == input)
    if(FindMemberOption.isEmpty) {println("찾는 정보가 없습니다.")
      ShowMenu()
    }
    else {
      println("찾기완료" + FindMemberOption.get)
      ShowMenu()
    }
  }

  def PhoneNumberSearch(input: String):Unit = {
    val FindMemberOption = Members.find(member => member.phoneNumber == input)
    if(FindMemberOption.isEmpty) {println("찾는 정보가 없습니다.")
      ShowMenu()
    }
    else {
      println("찾기완료" + FindMemberOption.get)
      ShowMenu()
    }
  }

  def AddressSearch(input: String):Unit = {
    val FindMemberOption = Members.find(member => member.address.get == input)
    if(FindMemberOption.isEmpty) {println("찾는 정보가 없습니다.")
      ShowMenu()
    }
    else {
      println("찾기완료" + FindMemberOption.get)
      ShowMenu()
    }
  }
  /* 변수만 다르고 반복되는걸 한개로 합치는 방법은 없을까
  def SearchProcess(input:String):Unit = {
    val FindMemberOption = Members.find(member => member.address.get == input)
    if(FindMemberOption.isEmpty) {println("찾는 정보가 없습니다.")
      ShowMenu()
    }
    else {
      println("찾기완료" + FindMemberOption.get)
      ShowMenu()
    }
  }*/

  //3. 삭제
  def DeleteMenu(): Unit = {
    println(
      """
        |1.이름으로 찾기
        |2.전화번호로 찾기
        |3.주소로 찾기
        |""".stripMargin
    )
    val input = scala.io.StdIn.readLine() match {
      case "1" => Delete("이름 입력", NameDelete)
      case "2" => Delete("전화번호 입력", PhoneNumberDelete)
      case "3" => Delete("주소 입력", AddressDelete)
    }
  }

  def Delete(message:String, func: String=> Unit): Unit = {
    println(message)
    val input = scala.io.StdIn.readLine()
    func(input)
  }
  def NameDelete(input: String):Unit = {
    val FindMemberOption = Members.find(member => member.name == input)
    if(FindMemberOption.isEmpty) {println("찾는 정보가 없습니다.")
      ShowMenu()
    }
    else {
      Members = Members.filterNot(member => member.name == input)
      println("삭제완료")
      ShowMenu()
    }
  }

  def PhoneNumberDelete(input: String):Unit = {
    val FindMemberOption = Members.find(member => member.phoneNumber == input)
    if(FindMemberOption.isEmpty) {println("찾는 정보가 없습니다.")
      ShowMenu()
    }
    else {
      Members = Members.filterNot(member => member.phoneNumber == input)
      println("삭제완료")
      ShowMenu()
    }
  }

  def AddressDelete(input: String):Unit = {
    val FindMemberOption = Members.find(member => member.address.get == input)
    if(FindMemberOption.isEmpty) {println("찾는 정보가 없습니다.")
      ShowMenu()
    }
    else {
      Members = Members.filterNot(member => member.address.get == input)
      println("삭제완료")
      ShowMenu()
    }
  }

  //5. 수정
  def UpdateMenu(): Unit = {
    println(
      """
        |1.이름으로 찾기
        |2.전화번호로 찾기
        |3.주소로 찾기
        |""".stripMargin
    )
    val input = scala.io.StdIn.readLine() match {
      case "1" => Update("이름 입력", NameUpdate)
      case "2" => Update("전화번호 입력", PhoneNumberUpdate)
      case "3" => Update("주소 입력", AddressUpdate)
    }
  }
  def Update(message:String, func: String=> Unit): Unit = {
    println(message)
    val input = scala.io.StdIn.readLine()
    func(input)
  }
  def NameUpdate(input:String):Unit = {
    val UpdateMemberOption = Members.find(member => member.name == input)
    if(UpdateMemberOption.isEmpty){
      println("찾는 정보가 없습니다.")
      ShowMenu()
    }
    else {
      val InputName = Input("수정할 이름입력", NameValidate).getOrElse("Nothing")
      val InputPhone = Input("수정할 번호입력", PhoneValidate).getOrElse("Nothing")
      println("수정할 주소입력")
      val InputAddress = Option(scala.io.StdIn.readLine())
      Members = Members.updated(Members.indexOf(UpdateMemberOption.get),Member(InputName,InputPhone,InputAddress))
      println("수정완료")
      ShowMenu()
    }
  }
  def PhoneNumberUpdate(input:String):Unit = {
    val UpdateMemberOption = Members.find(member => member.phoneNumber == input)
    if(UpdateMemberOption.isEmpty){
      println("찾는 정보가 없습니다.")
      ShowMenu()
    }
    else {
      val InputName = Input("수정할 이름입력", NameValidate).getOrElse("Nothing")
      val InputPhone = Input("수정할 번호입력", PhoneValidate).getOrElse("Nothing")
      println("수정할 주소입력")
      val InputAddress = Option(scala.io.StdIn.readLine())
      Members = Members.updated(Members.indexOf(UpdateMemberOption.get),Member(InputName,InputPhone,InputAddress))
      println("수정완료")
      ShowMenu()
    }
  }
  def AddressUpdate(input:String):Unit = {
    val UpdateMemberOption = Members.find(member => member.address.get == input)
    if(UpdateMemberOption.isEmpty){
      println("찾는 정보가 없습니다.")
      ShowMenu()
    }
    else {
      val InputName = Input("수정할 이름입력", NameValidate).getOrElse("Nothing")
      val InputPhone = Input("수정할 번호입력", PhoneValidate).getOrElse("Nothing")
      println("수정할 주소입력")
      val InputAddress = Option(scala.io.StdIn.readLine())
      Members = Members.updated(Members.indexOf(UpdateMemberOption.get),Member(InputName,InputPhone,InputAddress))
      println("수정완료")
      ShowMenu()
    }
  }

  //수정 끝

  //5.전체목록
  def ListProcess(): Unit = {
    println("전체목록")
    Members.foreach(member => println(member))
    println("")
    ShowMenu()

  }
  //전체목록 끝



  def main(args: Array[String]): Unit = {
    ShowMenu()
  }



}
