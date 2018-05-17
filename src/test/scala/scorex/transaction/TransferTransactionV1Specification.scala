package scorex.transaction

import com.wavesplatform.{TransactionGen, state}
import com.wavesplatform.state.ByteStr
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.{Address, AddressOrAlias, Alias, PublicKeyAccount}
import scorex.transaction.transfer._
import play.api.libs.json._

class TransferTransactionV1Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Transfer serialization roundtrip") {
    forAll(transferV1Gen) { transfer: TransferTransactionV1 =>
      val recovered = TransferTransactionV1.parseBytes(transfer.bytes()).get

      recovered.sender.address shouldEqual transfer.sender.address
      recovered.assetId.map(_ == transfer.assetId.get).getOrElse(transfer.assetId.isEmpty) shouldBe true
      recovered.feeAssetId.map(_ == transfer.feeAssetId.get).getOrElse(transfer.feeAssetId.isEmpty) shouldBe true
      recovered.timestamp shouldEqual transfer.timestamp
      recovered.amount shouldEqual transfer.amount
      recovered.fee shouldEqual transfer.fee
      recovered.recipient.stringRepr shouldEqual transfer.recipient.stringRepr

      recovered.bytes() shouldEqual transfer.bytes()
    }
  }

  property("Transfer serialization from TypedTransaction") {
    forAll(transferV1Gen) { tx: TransferTransactionV1 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("json format validation") {
    val js = """
      "type": 4,
    "id": "3RkWfTXd4akphqNi9ggoyDzs7XeAg63md9dior1ESbo8",
    "sender": "3MvbZaWThxGQ7YE3FA6jc7KLj6EXhhYS7Xy",
    "senderPublicKey": "88evu5gxTdCaR35eYr4roSCADDhhQmJDxHxZ9VQtudWV",
    "fee": 100000,
    "timestamp": 1526552510868,
    "signature": "2sqCfXfDLgqofMMkPPUWbp5kP4VcznmiGpStNE9MNNzv6ZTjFBPpfaJDfhp9hf2aoZWVrbRWNKWbMT6hwYkSp974",
    "version": 1,
    "recipient": "3MzfTZcrLjntFY1jLMUcSgkzhg349v2MmZX",
    "assetId": null,
    "feeAssetId": null,
    "amount": 1900000,
    "attachment": ""
    """

    val tx1 = TransferTransactionV1
      .create(
        None,
        PublicKeyAccount.fromBase58String("88evu5gxTdCaR35eYr4roSCADDhhQmJDxHxZ9VQtudWV").right.get,
        Address.fromString("3MzfTZcrLjntFY1jLMUcSgkzhg349v2MmZX").right.get,
        1900000,
        BigInt(1526552510868L).longValue(),
        None,
        100000,
        "".getBytes,
        ByteStr("2sqCfXfDLgqofMMkPPUWbp5kP4VcznmiGpStNE9MNNzv6ZTjFBPpfaJDfhp9hf2aoZWVrbRWNKWbMT6hwYkSp974".getBytes)
      )
      .right
      .get

    val j = tx1.json()
    js shouldEqual j
  }
}
