package module3

import module3.zioConcurrency.printEffectRunningTime
import module3.zio_homework.config.{AppConfig, load}
import module3.zio_homework.effectService.EffectService
import zio.{Has, Task, ULayer, ZIO, ZLayer, ZManaged}
import zio.clock.{Clock, sleep}
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */



  lazy val guessProgram: ZIO[Console with Random, IOException, Unit] = {
    def getNum: ZIO[Console, IOException, Int] = for {
      line <- getStrLn
      num <- ZIO.effect(line.toInt).orElse(putStrLn("It's not a number!") *> getNum)
    } yield num

    for {
      randomNum <- nextIntBetween(1,3)
      _ <- putStrLn("What number from 1 to 3 did i guess?")
      userNum <- getNum
      _ <- putStrLn(if (userNum == randomNum) "Goo job!" else "Guessed wrong(")
    } yield ()
  }

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile[R, E](effect: ZIO[R, E, Boolean]): ZIO[R, E, Boolean] = {
    ZIO.ifM(effect)(effect, effect *> doWhile(effect))
  }

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */

  def loadConfigOrDefault: ZIO[Any, Nothing, AppConfig] = load.orElse(ZIO.succeed(AppConfig("default", "default")))

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: ZIO[Random with Clock, Nothing, Int] = nextIntBetween(0, 10).delay(1.seconds)

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: Seq[ZIO[Random with Clock, Nothing, Int]] = List.fill(10)(eff)

  
  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app: ZIO[Console with Clock with Random, Nothing, Seq[Int]] = {
    val sum: ZIO[Random with Clock, Nothing, Seq[Int]] = ZIO.collectAll(effects)
    for {
      res <- printEffectRunningTime(sum)
      _ <- putStrLn(s"Result: ${res.sum}")
    } yield res
  }
  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp = {
    val sum: ZIO[Random with Clock, Nothing, Seq[Int]] = ZIO.collectAllPar(effects)
    for {
      res <- printEffectRunningTime(sum)
      _ <- putStrLn(s"Result: ${res.sum}")
    } yield res
  }


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

  object effectService {

    type EffectService = Has[EffectService.Service]
//@accessible            не компилируется с макросом, что сделал не так?
    object EffectService {

      trait Service {
        def printRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A]
      }

      val live = ZLayer.succeed(new Service {
        override def printRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A] =
          printEffectRunningTime(zio)
      })

      def printRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[EffectService with Console with Clock with R, E, A] =
        ZIO.accessM(_.get.printRunningTime(zio))
    }
  }


   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg: ZIO[Console with Clock with Random with EffectService, Nothing, Seq[Int]] = {
    val sum: ZIO[Random with Clock, Nothing, Seq[Int]] = ZIO.collectAll(effects)
    for {
      res <- EffectService.printRunningTime(sum)
      _ <- putStrLn(s"Result: ${res.sum}")
    } yield res
  }

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */

  lazy val runApp: ZIO[Any, Nothing, Seq[Int]] = {
    val layer = Console.live ++ Clock.live ++ Random.live ++ EffectService.live
    appWithTimeLogg.provideLayer(layer)
  }

}
