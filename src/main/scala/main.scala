import scala.util.matching.Regex
import scala.io.StdIn.readLine
object main {

  private val hangeulPattern: Regex = "[^가-힣]".r
  private val numberPattern: Regex = "[^0-9]".r
  private var Members = List[Member]()
  case class Member(name: String, phoneNumber: String, address:Option[String]){

    private val strAddresss = address match {
    case Some(strAddress) => strAddress
    case None =>
  }
    override def toString: String = {

      return "이름 : "+ name + " 전화번호: " + phoneNumber+ " 주소: " + strAddresss
    }
  }
  // print(member.toString)
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
      case "2" => readUpdateDeleteMenu("search") //read,update,delete menu
      case "3" => println("삭제 대상 찾기")
        readUpdateDeleteMenu("delete") //read,update,delete menu
      case "4" => printMembers()
      case "5" => println("수정 대상 찾기")
        readUpdateDeleteMenu("update") //read,update,delete menu
      case "6" => System.exit(0);
      case _ => println("입력오류")
        println("1~6 사이의 숫자를 입력해주세요")
        showMenu();
    }
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

  def input(message:String , validate :String => Option[String]):Option[String] = {
    println(message)
    val inputValue = readLine()
    validate(inputValue)
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

  //read,update,delete menu
  def readUpdateDeleteMenu(rudKey: String): Unit = {
    println(
      """
        |1.이름으로 찾기
        |2.전화번호로 찾기
        |3.주소로 찾기
        |""".stripMargin
    )
    readLine() match {
      case "1" => gennericFunc(rudKey,findMember("이름 입력","nameFind"))
      case "2" => gennericFunc(rudKey,findMember("전화번호 입력","phoneNumberFind"))
      case "3" => gennericFunc(rudKey,findMember("주소 입력","addressFind"))
    }
  }
  def findMember(message:String,findKind: String): Option[Member]= {
    println(message)
    val inputString= readLine()
    findKind match {
      case "nameFind" => Members.find(member => member.name == inputString)
      case "phoneNumberFind" => Members.find(member => member.phoneNumber == inputString)
      case "addressFind" => Members.find(member => member.address == Option(inputString))
      case _ => Option(Member("","",Option("")))
    }
  }

  def gennericFunc(funcKind:String, findMemberOption: Option[Member]) :Unit= {
    funcKind match {
      case "search" => search(findMemberOption)
      case "delete" => delete(findMemberOption)
      case "update" => update(findMemberOption)
      case _ => {
        println("오류")
        showMenu()
      }
    }
  }
  def search(findMemberOption: Option[Member]): Unit = {
    findMemberOption match {
      case Some(value) =>
        println("찾기완료")
        println(value.toString)
        showMenu()
      case _ => println("찾는 정보가 없습니다.")
        showMenu()
    }
  }
  def delete(findMemberOption: Option[Member]): Unit = {
    findMemberOption match {
      case Some(value) =>
        Members = Members.filterNot(member => member==value)
        println("삭제완료")
        showMenu()
      case _ => println("찾는 정보가 없습니다.")
        showMenu()
    }
  }

  def update(findMemberOption: Option[Member]): Unit = {
    findMemberOption match {
      case Some(value) =>
        val inputName = input("수정할 이름입력", validateName).getOrElse("Nothing")
        val inputPhone = input("수정할 번호입력", updateValidatePhone).getOrElse("Nothing")
        println("수정할 주소입력")
        val inputAddress = Option(readLine())
        Members = Members.updated(Members.indexOf(value),Member(inputName,inputPhone,inputAddress))
        println("수정완료")
        showMenu()
      case _ => println("찾는 정보가 없습니다.")
        showMenu()
    }
  }
  def updateValidatePhone(inputPhoneNumber: String): Option[String] = { //전화번호 숫자검사
    numberPattern.findFirstMatchIn(inputPhoneNumber) match {
      case None => println("숫자확인")
        duplicatePhone(inputPhoneNumber)
      case Some(_) => println("숫자만 입력해주세요")
        input("번호입력",validatePhone)
      case _ => println("오류")
        input("번호입력",validatePhone)
    }
  }


  //5.전체목록
  def printMembers(): Unit = {
    println("전체목록")
    Members.foreach(member => println(member.toString))
    println("")
    showMenu()

  }
  //전체목록 끝

  def main(args: Array[String]): Unit = {
    showMenu()
  }



}
