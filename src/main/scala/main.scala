import java.util.Scanner
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

object main {

  private val sc = new Scanner(System.in);
  private val MemberList = ArrayBuffer[Member]();
  private val HangeulPattern: Regex = "[^가-힣]".r
  private val NumberPattern: Regex = "[^0-9]".r
  private val lastid = 1

  private class Member {
    private var _name = ""
    private var _phonenumber = ""
    private var _id = 1
    def name: String = _name

    def name_=(newValue: String): Unit = {
      _name = newValue
    }

    def phonenumber: String = _phonenumber

    def id: Int = _id

    //데이터가 모두 삭제되었을때는 고유id가 아니고 다시 1로 시작되는것 생각하기
    def phonenumber_=(newValue: String): Unit = {
      println("세터 폰넘버"+_phonenumber)
      if(_phonenumber.equals("")){
      if(MemberList.isEmpty) {_id = id
        println("엠티 id 생성")}
      else {
        println("else id 생성")
        _id += _id
      }

      }
      _phonenumber = newValue
    }

  }

  //시작화면
  def Begin(): Unit = {
    println("전화번호부")
    println("1.등록")
    println("2.찾기")
    println("3.삭제")
    println("4.전체목록")
    println("5.수정")
    println("6.종료")
    val key = sc.nextLine()
    keymatchTest(key);
  }

  def keymatchTest(key: String) = key match {
    case "1" => RegisterProcess()
    case "2" => SearchProcess()
    case "3" => println("삭제 대상 찾기")
                SearchProcess()
    case "4" => ListProcess()
    case "5" => println("수정 대상 찾기")
                SearchProcess()
    case "6" => System.exit(0);
    case _ => println("입력오류")
      println("1~6 사이의 숫자를 입력해주세요")
      println("")
      Begin();
  }
  //시작화면 끝

  //1.등록
  def RegisterProcess(): Unit = {
    val RegisterName = NameMatchTest(Registername())
    val RegisterPhonenumber = RegisterPhoneMatchTest(PhonenumberLength())
    val Checkreturn = Check(RegisterName, RegisterPhonenumber);
    CheckMatchTest(Checkreturn, RegisterName, RegisterPhonenumber);
  }

  def Registername(): String = {
    println("이름입력")
    val RegisterName = sc.nextLine()
    if(RegisterName.length==1) { println("이름은 두글자이상")
                          Registername() }
    else RegisterName
  }

  def NameMatchTest(RegisterName: String): String = {
    HangeulPattern.findFirstMatchIn(RegisterName) match {
      case None => println("한글확인")
        RegisterName
      case Some(_) => println("한글만 입력해주세요")
        NameMatchTest(Registername());
    }
  }


  def PhonenumberLength(): String = {
    println("전화번호 입력")
    val RegisterPhonenumber = sc.nextLine()
    if (RegisterPhonenumber.length==11) RegisterPhonenumber
    else {println("전화번호는 11자리")
      PhonenumberLength();
    }
  }

  def RegisterPhoneMatchTest(RegisterPhonenumber: String): String = {
    NumberPattern.findFirstMatchIn(RegisterPhonenumber) match {
      case None => println("숫자확인")
        RegisterPhonenumber
      case Some(_) => println("숫자만 입력해주세요")
        RegisterPhoneMatchTest(PhonenumberLength());
    }
  }


    def Check(RegisterName: String, RegisterPhonenumber: String): String = {
      println("최종확인")
      println("이름 : " + RegisterName)
      println("전화번호 : " + RegisterPhonenumber)
      println("입력하신 정보가 맞으면 y 틀리면 n을 입력해주세요.")
      val Checkreturn = sc.nextLine()
      Checkreturn
    }

    //변수가 긴 이유는 이름과 전화번호를 받아서 입력오류시 다시 Check 메소드를 호출하기 위함
    def CheckMatchTest(Checkreturn: String, RegisterName: String, RegisterPhonenumber: String): Unit = {
      Checkreturn match {
        case "y"|"ㅛ" =>
          val member = new Member
          member.name = RegisterName
          member.phonenumber = RegisterPhonenumber
          MemberList.addOne(member)
          println("등록완료")
          println("")
          Begin()
        case "n"|"ㅜ" =>
          println("재입력")
          RegisterProcess();
        case _ => println("입력오류, 다시 입력해주세요")
          println("")
          CheckMatchTest(Check(RegisterName, RegisterPhonenumber), RegisterName, RegisterPhonenumber)
      }
    }
    //등록 끝

    //2. 찾기
    def SearchProcess():Unit = {
      println("1. 이름으로 찾기")
      println("2. 전화번호로 찾기")
      val SearchKey = sc.nextLine();
      SearchKey match {
        case "1" => NameSearchProcess();
        case "2" => PhoneNumberSearchProcess();
        case _ => println("입력오류, 다시 입력해주세요.")
                SearchProcess()
      }
    }
    //이름은 동명이인이 있을 수도 있으니 id로 구분
    def NameSearchProcess() = {
      val TempIdList = ArrayBuffer[Int]()
      println("이름 입력")
      val SearchName = sc.nextLine()
      MemberList.foreach(Member => Member.name match {
        case SearchName => val id = Member.id
          TempIdList.addOne(id);
          println("id: " + id + " "+"이름 : " + Member.name + " " + "전화번호 : " + Member.phonenumber)
        case _ =>
      })
      println("")

      //동명이인이 있을시 id로 구분
      if(TempIdList.length >= 2) {
        ChoiceId()
        def ChoiceId():Unit ={
        println("삭제하거나 수정할 id 입력")
        val Choiceid = sc.nextLine().toInt
        if(TempIdList.contains(Choiceid)) {
          DeleteOrUpdateProcesse(Choiceid)
        }
        else {
          println("잘못된 입력입니다 다시 입력해주세요,")
          ChoiceId()
        }
        }

      }
      else { println(TempIdList(0))
        DeleteOrUpdateProcesse(TempIdList(0))}

    }

    def DeleteOrUpdateProcesse(id:Int):Unit = {
      println("1.삭제")
      println("2.수정")
      println("3.돌아가기")
      val UpdateMember = sc.nextLine()
      UpdateMember match {
        case "1" => DeleteProcess(id)
        case "2" => UpdateProcess(id)
        case "3" =>Begin();
        case _ => println("입력오류, 재입력해주세요.")
          DeleteOrUpdateProcesse(id);
      }
    }

    def PhoneNumberSearchProcess() = {
      println("전화번호 입력")
      val TempIdList = ArrayBuffer[Int]()
      val SearchPhonenumber = sc.nextLine()
      SearchFindIndex();
      def SearchFindIndex() = {
        MemberList.foreach(Member => Member.phonenumber match {
          case SearchPhonenumber => val id = Member.id
            TempIdList.addOne(id);
            println("id: " + id + "이름 : " + Member.name + " " + "전화번호 : " + Member.phonenumber)
            MemberList.indexOf(Member);
          case _ =>
        })
      }
      println(TempIdList(0))
      DeleteOrUpdateProcesse(TempIdList(0));
    }

    //찾기 끝

    //3. 삭제
    def DeleteProcess(id: Int):Unit = {
      val TempMemberList = ArrayBuffer[Member]();
      MemberList.foreach(Member => Member.id match {
        case id => val DeleteMember = Member
                  TempMemberList.addOne(DeleteMember)
        case _ =>
      })
      MemberList.remove(MemberList.indexOf(TempMemberList(0)))
      println("삭제완료")
      println("")
      Begin();
    }
    //삭제 끝

    //4. 수정
    def UpdateProcess(id: Int):Unit = {
      val TempMemberList = ArrayBuffer[Member]();
      MemberList.foreach(Member => Member.id match {
        case id => val UpdateMember = Member
          TempMemberList.addOne(UpdateMember)
        case _ =>
      })
      println("수정할 이름을 입력해주세요.")
      val UpdateName = sc.nextLine()
      if(UpdateName.length==1) { println("이름은 두글자이상")
        UpdateProcess(id) }
      else UpdateName
      NameMatchTest(UpdateName)
      println("수정할 전화번호를 입력해주세요")
      val UpdatePhonenumber = sc.nextLine()
      if (UpdatePhonenumber.length==11) UpdatePhonenumber
      else {println("전화번호는 11자리")
        PhonenumberLength();
      }
      RegisterPhoneMatchTest(UpdatePhonenumber)
      val UpdateMember = TempMemberList(0)
      val Checkreturn = Check(UpdateName, UpdatePhonenumber);
      UpdateCheckMatchTest(Checkreturn, UpdateMember, UpdateName, UpdatePhonenumber);

    }

  def UpdateCheckMatchTest(Checkreturn: String,UpdateMember: Member, UpdateName: String, UpdatePhonenumber: String): Unit = {
    Checkreturn match {
      case "y"|"ㅛ" =>
        UpdateMember.name = UpdateName
        UpdateMember.phonenumber = UpdatePhonenumber
        MemberList.update(MemberList.indexOf(UpdateMember),UpdateMember)
        /*
        println("멤버이름 " + member.name)
        println("멤버폰번호 " + member.phonenumber)
        println("멤버 등록 확인" + MemberList(0).name + MemberList(0).phonenumber)*/
        println("수정완료")
        println("")
        Begin()
      //데이터 저장하기 추가 해야됨
      case "n"|"ㅜ" =>
        println("재입력")
        UpdateProcess(UpdateMember.id);
      case _ => println("입력오류, 다시 입력해주세요")
        println("")
        UpdateCheckMatchTest(Checkreturn, UpdateMember, UpdateName, UpdatePhonenumber);
    }
  }

    //수정 끝

    //5.전체목록
    def ListProcess(): Unit = {
      println("전체목록")
      MemberList.foreach(Member => println("이름 : " + Member.name + " " + "전화번호 : " + Member.phonenumber))
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
