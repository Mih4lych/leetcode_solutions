package learning.cats.rvjexercises

object CatsExReader extends App {
  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)

  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched" // select * from the db table and return the status of the orderID

    def getLastOrderId(username: String): Long = 542643 // select max(orderId) from table where username = username
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started") // this would start the actual server
  }

  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) = s"From: $emailReplyTo; to: $address >>> $contents"
  }

  // bootstrap
  val config = Configuration("daniel", "rockthejvm1!", "localhost", 1234, 8, "daniel@rockthejvm.com")
  // cats Reader

  import cats.data.Reader

  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))
  val dbConn = dbReader.run(config)

  // Reader[I, O]
  val danielsOrderStatusReader: Reader[Configuration, String] = dbReader.map(dbcon => dbcon.getOrderStatus(55))
  val danielsOrderStatus: String = danielsOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    val usersLastOrderIdReader: Reader[Configuration, String] = dbReader
      .map(_.getLastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))

    // identical:
    val usersOrderFor: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus

    usersOrderFor.run(config)
  }

  // email a user
  def emailUser(username: String, userEmail: String): String = {
    // fetch the status of their last order
    // email them with the Email service: "Your last order has the status: (status)"
    val emailServiceReader: Reader[Configuration, EmailService] = Reader(config => EmailService(config.emailReplyTo))

    val emailReader =
      for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      emailService <- emailServiceReader
    } yield emailService.sendEmail(userEmail, orderStatus)

    emailReader.run(config)
  }
}
