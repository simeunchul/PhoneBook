import java.util.Scanner
import scala.::
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

object main {

  private val sc = new Scanner(System.in);
  private val HangeulPattern: Regex = "[^가-힣]".r
  private val NumberPattern: Regex = "[^0-9]".r
  private var Members = List[Member]()
  private case class Member(name: String, PhoneNumber: String)


  //시작화면
  def Begin(): Unit = {
    println("전화번호부")
    println("1.등록")
    println("2.찾기")
    println("3.삭제")
    println("4.전체목록")
    println("5.수정")
    println("6.종료")
    val key = sc.nextLine().toInt
    keymatchTest(key);
  }

  def keymatchTest(key: Int) = key match {
    case 1 => RegisterProcess(key)
    case 2 => SearchProcess(key)
    case 3 => println("삭제 대상 찾기")
      SearchProcess(key)
    case 4 => ListProcess()
    case 5 => println("수정 대상 찾기")
      SearchProcess(key)
    case 6 => System.exit(0);
    case _ => println("입력오류")
      println("1~6 사이의 숫자를 입력해주세요")
      println("")
      Begin();
  }
  //시작화면 끝

  //1.등록
  def RegisterProcess(key:Int): Unit = {
    val RegisterName = NameMatchTest(Registername(key),key)
    val RegisterPhonenumber = RegisterPhoneMatchTest(PhonenumberLength())
    val Checkreturn = check(RegisterName, RegisterPhonenumber);
    checkMatchTest(Checkreturn, RegisterName, RegisterPhonenumber, key);
  }

  def Registername(key:Int): String = {
    if(key==1) println("이름 입력")
    if(key==5) println("수정할 이름 입력")
    val RegisterName = sc.nextLine()
    if (RegisterName.length == 1) {
      println("이름은 두글자이상")
      Registername(key)
    }
    else RegisterName
  }

  def NameMatchTest(RegisterName: String, key:Int): String = {
    HangeulPattern.findFirstMatchIn(RegisterName) match {
      case _ => println("한글확인")
        RegisterName
      case Some(_) => println("한글만 입력해주세요")
        NameMatchTest(Registername(key),key);
    }
  }


  def PhonenumberLength(): String = {
    println("전화번호 입력")
    val RegisterPhonenumber = sc.nextLine()
    if (RegisterPhonenumber.length == 11) RegisterPhonenumber
    else {
      println("전화번호는 11자리")
      PhonenumberLength();
    }
  }

  def RegisterPhoneMatchTest(RegisterPhonenumber: String): String = {
    NumberPattern.findFirstMatchIn(RegisterPhonenumber) match {
      case _ => println("숫자확인")
        RegisterPhonenumber
      case Some(_) => println("숫자만 입력해주세요")
        RegisterPhoneMatchTest(PhonenumberLength());
    }
  }


  def check(RegisterName: String, RegisterPhonenumber: String): String = {
    println("최종확인")
    println("이름 : " + RegisterName)
    println("전화번호 : " + RegisterPhonenumber)
    println("입력하신 정보가 맞으면 y 틀리면 n을 입력해주세요.")
    val Checkreturn = sc.nextLine()
    Checkreturn
  }

  //변수가 긴 이유는 이름과 전화번호를 받아서 입력오류시 다시 Check 메소드를 호출하기 위함
  def checkMatchTest(Checkreturn: String, RegisterName: String, RegisterPhonenumber: String, key:Int): Unit = {
    Checkreturn match {
      case "y" | "ㅛ" =>
        val member = Member(RegisterName, RegisterPhonenumber)
        println(member)
        Members = Members :+ member
        println("등록완료")
        println("")
        Begin()
      case "n" | "ㅜ" =>
        println("재입력")
        RegisterProcess(key);
      case _ => println("입력오류, 다시 입력해주세요")
        println("")
        checkMatchTest(check(RegisterName, RegisterPhonenumber), RegisterName, RegisterPhonenumber, key)
    }
  }
  //등록 끝

  //2. 찾기
  def SearchProcess(int: Int): Unit = {
    println("1. 이름으로 찾기")
    println("2. 전화번호로 찾기")
    val SearchKey = sc.nextLine();
    SearchKey match {
      case "1" => NameSearchProcess(int);
      case "2" => PhoneNumberSearchProcess(int);
      case _ => println("입력오류, 다시 입력해주세요.")
        SearchProcess(int)
    }
  }

  //이름은 동명이인이 있을 수도 있으니 id로 구분
  def NameSearchProcess(int: Int) = {
    var TempIdList = List[Int]()
    println("이름 입력")
    val SearchName = sc.nextLine()
    Members.foreach(Member => Member.name match {
      case SearchName =>
        val name = Member.name
        val phonenumber = Member.PhoneNumber
        val id = Members.indexOf(Member)
        TempIdList = TempIdList :+ id
      case _ =>
    })
    if (TempIdList.isEmpty) {
      println("찾는 정보가 없습니다.")
      println("")
      Begin()
    }
    else {
      TempIdList.foreach(id => println(s"id: $id 이름: ${Members(id).name} 전화번호: ${Members(id).PhoneNumber}"))
      println("찾기완료")
      println("")
    }
    if (int == 2) Begin()
    if (int == 3 || int == 5) sameNamecheck(TempIdList, int)
  }



  def PhoneNumberSearchProcess(int: Int) = {
    println("전화번호 입력")
    var tempid: Int = -1;
    val SearchPhonenumber = sc.nextLine()
    Members.foreach(Member => Member.PhoneNumber match {
      case SearchPhonenumber =>
        val name = Member.name
        val phonenumber = Member.PhoneNumber
        tempid = Members.indexOf(Member)
        println(s"id: $tempid 이름: $name 전화번호: $phonenumber")
        Member
      case _ =>
    })
    if (tempid == -1) {
      println("찾는 정보가 없습니다.")
      Begin()
    }
    if (int == 2) Begin()
    if (int == 3) DeleteProcess(tempid)
    if (int == 5) UpdateProcess(tempid, int:Int)
  }

  //찾기 끝

  //3. 삭제
  def DeleteProcess(id: Int): Unit = {
    println("정말로 삭제 하시겠습니까? y/n")
    val deletecheck = sc.nextLine()
    deletecheck match {
      case "y" =>
        Members = Members.filterNot(Member => Members.indexOf(Member) == id)
        println("삭제완료")
        println("")
        Begin();
      case "n" => println("처음으로 돌아갑니다.")
        println("")
        Begin();
      case _ => println("잘못된 입력입니다.")
        DeleteProcess(id)
    }
  }

  def sameNamecheck(Tempidlist: List[Int], key: Int): Unit = {
    if(Tempidlist.length ==1){
      if (key == 3) DeleteProcess(Tempidlist.head)

      if (key == 5 ) UpdateProcess(Tempidlist.head,key)
    }
    if(key==3){
      println("삭제할 사람의 id를 입력해 주세요")
      val deleteid = sc.nextLine().toInt
      DeleteProcess(deleteid)
    }
    if(key==5){
      println("수정할 사람의 id를 입력해 주세요")
      val updateid = sc.nextLine().toInt
      UpdateProcess(updateid,key)
    }
  }

    //삭제 끝

    //4. 수정
    def UpdateProcess(id: Int, int:Int):Unit = {
      val UpdateName = NameMatchTest(Registername(int),int)
      val UpdatePhonenumber = RegisterPhoneMatchTest(PhonenumberLength())
      val Checkreturn = check(UpdateName, UpdatePhonenumber);
      UpdateCheckMatchTest(Checkreturn, id, UpdateName, UpdatePhonenumber,int);

    }

  def UpdateCheckMatchTest(Checkreturn: String, id : Int, UpdateName: String, UpdatePhonenumber: String, int:Int): Unit = {
    Checkreturn match {
      case "y"|"ㅛ" =>
        val UpdateMember = Member(UpdateName,UpdatePhonenumber)
        Members = Members.updated(id,UpdateMember)
        println(Members(id))
        println("수정완료")
        println("")
        Begin()
      //데이터 저장하기 추가 해야됨
      case "n"|"ㅜ" =>
        println("재입력")
        UpdateProcess(id,int);
      case _ => println("입력오류, 다시 입력해주세요")
        println("")
        UpdateCheckMatchTest(Checkreturn, id, UpdateName, UpdatePhonenumber,int);
    }
  }

    //수정 끝

    //5.전체목록
    def ListProcess(): Unit = {
      println("전체목록")
      Members.foreach(Member => println(s"id : ${Members.indexOf(Member)} 이름 : ${Member.name} 전화번호 : ${Member.PhoneNumber}"))
      println("처음으로 돌아가시려면 아무거나 입력해주세요.")
      val again = sc.nextLine()
      again match {
        case _ => Begin()
      }
    }
    //전체목록 끝



    def main(args: Array[String]): Unit = {
      Begin()
    }



}
