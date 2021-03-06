package fintech.homework01
import scala.collection.immutable

// Используя функции io.readLine и io.printLine напишите игру "Виселица"
// Пример ввода и тест можно найти в файле src/test/scala/fintech/homework01/HangmanTest.scala
// Тест можно запустить через в IDE или через sbt (написав в консоли sbt test)

// Правила игры "Виселица"
// 1) Загадывается слово
// 2) Игрок угадывает букву
// 3) Если такая буква есть в слове - они открывается
// 4) Если нет - рисуется следующий элемент висельника
// 5) Последней рисуется "веревка". Это означает что игрок проиграл
// 6) Если игрок все еще жив - перейти к пункту 2

// Пример игры:

// Word: _____
// Guess a letter:
// a
// Word: __a_a
// Guess a letter:
// b
// +----
// |
// |
// |
// |
// |

// и т.д.

class Hangman(io: IODevice) {


  def play(word: String): Unit = {
    var secret = "_" * word.length
    var currentStage = stages
    while (currentStage.tail != Nil && secret.contains('_')) {
      io printLine
        currentStage.head +
        s"Word: $secret\n" +
       "Guess a letter:"

      val letter = nextLetter()
      var fail = true

      secret = secret zip word map {
        case (_, w) if w == letter =>
          fail = false
          w
        case (s, _) => s
      } mkString

      if(fail)
        currentStage = currentStage.tail
    }
    io printLine currentStage.head +
      (if(currentStage.tail == Nil)
        "You are dead"
      else
        s"ok! $word")

  }

  def nextLetter():Char = {
    val str = io readLine()
    if (str.length == 1)
      return str.head
    io printLine "Incorrect input, try again:"
    nextLetter()
  }

  val stages = List(
    "",
    """+----
      ||
      ||
      ||
      ||
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||
      ||
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||   |
      ||
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||   |
      ||  /
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||   |
      ||  / \
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||  /|
      ||  / \
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||  /|\
      ||  / \
      ||
      |""".stripMargin,
    """+----
      ||   |
      ||   O
      ||  /|\
      ||  / \
      ||
      |""".stripMargin
  )
}

trait IODevice {
  def printLine(text: String): Unit
  def readLine(): String
}
