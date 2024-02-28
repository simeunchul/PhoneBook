import PhoneNumberBook.Member

import scala.util.matching.Regex
import scala.io.StdIn.readLine
object main {

  private val hangeulPattern: Regex = "[^가-힣]".r
  private val numberPattern: Regex = "[^0-9]".r
  private var Members = List[Member]()
  // print(member.toString)
  //시작화면
  def showMenu(): Unit = {
    println(
      """
        |PhoneBook
        |1.register
        |2.search
        |3.delete
        |4.printMebers
        |5.update
        |6.exit
        |""".stripMargin
    )
    readLine() match {
      case "1" => register()
      case "2" => readUpdateDeleteMenu("search") //read,update,delete menu
      case "3" => println("search member for delete")
        readUpdateDeleteMenu("delete") //read,update,delete menu
      case "4" => printMembers()
      case "5" => println("search member for update")
        readUpdateDeleteMenu("update") //read,update,delete menu
      case "6" => System.exit(0);
      case _ => println("error")
        println("input number 1~6")
        showMenu();
    }
  }

  //1.등록
  def register(): Unit = {
    val inputName = input("inputName", validateName)
    val inputPhoneNumber = input("inputPhoneNumber", validatePhoneNumber)
    println("inputAddress")
    val inputAddress = readLine()
    val member = Member(inputName,inputPhoneNumber,Option(inputAddress))
    Members = Members :+ member
    println("regist")
    showMenu()
  }


  def validateName(inputName:String): String = { // 이름 한글검사
    hangeulPattern.findFirstMatchIn(inputName)
      .map{ value =>
      println("hangeulOnly")
        input("inputName", validateName)
      }
      .getOrElse {
        println("hangeulCheck")
        inputName
      }
  }

  def validatePhoneNumber(inputPhoneNumber: String): String = { //전화번호 숫자검사
    numberPattern.findFirstMatchIn(inputPhoneNumber)
      .map{ value =>
        println("numberOnly")
        input("inputPhoneNumber",validatePhoneNumber)
      }
      .getOrElse{
        println("numberCheck")
        duplicatePhoneNumber(inputPhoneNumber)
      }
  }

  def input(message:String , validate :String => String): String = {
    println(message)
    val inputValue = readLine()
    validate(inputValue)
  }



  def duplicatePhoneNumber(inputPhoneNumber:String): String ={
      Members.find(member => member.phoneNumber==inputPhoneNumber)
        .map{ value =>
          println("duplicate")
          input("inputPhoneNumber",validatePhoneNumber)
        }
        .getOrElse(inputPhoneNumber)
  }

  def updatValidatePhoneNumber(inputPhoneNumber: String, updateId: Int): String = { //전화번호 숫자검사
    numberPattern.findFirstMatchIn(inputPhoneNumber)
      .map{ value =>
        println("numberOnly")
        input("inputPhoneNumber",validatePhoneNumber)
      }
      .getOrElse{
        println("numberCheck")
        updateDuplicatePhoneNumber(inputPhoneNumber,updateId)
      }
  }

  def removeSelfElement(updateId: Int): List[Member] = {
    Members.filterNot(member => Members.indexOf(member) == updateId)
  }
  def updateDuplicatePhoneNumber(inputPhoneNumber:String, updateId: Int): String ={
    removeSelfElement(updateId)
    .find(member => member.phoneNumber==inputPhoneNumber)
      .map{ value =>
        println("duplicate")
        input("inputPhoneNumber",validatePhoneNumber)
      }
      .getOrElse(inputPhoneNumber)
  }
  //read,update,delete menu
  def readUpdateDeleteMenu(readUpdateDeleteKind: String): Unit = {
    println(
      """
        |1.search by name
        |2.search by PhoneNumber
        |3.search by address
        |""".stripMargin
    )
    readLine() match {
      case "1" => gennericFunc(readUpdateDeleteKind,findMember("input Name","nameFind"))
      case "2" => gennericFunc(readUpdateDeleteKind,findMember("input PhoneNumber","phoneNumberFind"))
      case "3" => gennericFunc(readUpdateDeleteKind,findMember("input Address","addressFind"))
    }
  }
  def findMember(message:String,findKind: String): Option[Member]= {
    println(message)
    val inputString= readLine()
    findKind match {
      case "nameFind" => Members.find(member => member.name == inputString)
      case "phoneNumberFind" => Members.find(member => member.phoneNumber == inputString)
      case "addressFind" => Members.find(member => member.address == Option(inputString))
    }
  }

  def gennericFunc(funcKind:String, findMemberOption: Option[Member]) :Unit= {
    funcKind match {
      case "search" => search(findMemberOption)
      case "delete" => delete(findMemberOption)
      case "update" => update(findMemberOption)
    }
  }
  def search(findMemberOption: Option[Member]): Unit = {
    findMemberOption
      .map{ value =>
        println("search")
        println(value.toString)
        showMenu()
      }
      .getOrElse{
        println("noSearchData")
        showMenu()
      }
  }
  def delete(findMemberOption: Option[Member]): Unit = {
    findMemberOption
      .map{ value =>
        Members = Members.filterNot(member => member==value)
        println("delete")
        showMenu()
      }
      .getOrElse{
        println("noData")
        showMenu()
      }
  }

  def update(findMemberOption: Option[Member]): Unit = {
    findMemberOption
      .map { value =>
        val updateId = Members.indexOf(value)
        val inputName = input("inputName for update", validateName)
        val inputPhone = input("inputPhoneNumber for update", updatValidatePhoneNumber(_,updateId))
        println("inputAddress for update")
        val inputAddress = readLine
        Members = Members.updated(updateId,
          Member(inputName, inputPhone, Option(inputAddress)))
        println("update")
        showMenu()
      }
      .getOrElse {
        println("noData")
        showMenu()
      }
  }

  //5.전체목록
  def printMembers(): Unit = {
    println("printMembers")
    Members.foreach(member => println(member.toString))
    println("")
    showMenu()

  }
  //전체목록 끝
  def main(args: Array[String]): Unit = {
    showMenu()
  }
}
