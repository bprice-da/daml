// Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.engine.script
package test

import com.daml.ledger.api.domain
import com.daml.ledger.api.testing.utils.SuiteResourceManagementAroundAll
import com.daml.lf.data.{ImmArray, Ref}
import com.daml.lf.engine.script.ledgerinteraction.ScriptTimeMode
import com.daml.lf.integrationtest.CantonFixture
import com.daml.lf.value.Value
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.Future
import scala.util.{Failure, Success}

final class AuthIT
    extends AsyncWordSpec
    with AbstractScriptTest
    with Matchers
    with SuiteResourceManagementAroundAll {
  import CantonFixture._
  import AbstractScriptTest._

  override protected lazy val authSecret = Some("secret")
  override protected lazy val darFiles = List(stableDarPath)
  override protected lazy val devMode = false
  override protected lazy val nParticipants = 1
  override protected lazy val timeMode = ScriptTimeMode.WallClock
  override protected lazy val tlsEnable = false

  "Daml Script against authorized ledger" can {
    "auth" should {
      "create and accept Proposal" in {
        for {
          adminClient <- defaultLedgerClient(getToken(adminUserId))
          userId = Ref.UserId.assertFromString(freshUserId())
          partyDetails <- Future.sequence(
            List.fill(2)(adminClient.partyManagementClient.allocateParty(None, None))
          )
          parties = partyDetails.map(_.party)
          user = domain.User(userId, None)
          rights = parties.map(domain.UserRight.CanActAs(_))
          _ <- adminClient.userManagementClient.createUser(user, rights)
          // we double check authentification is on
          wrongToken = getToken(userId, Some("not secret"))
          err <- scriptClients(token = wrongToken).transform {
            case Failure(err) => Success(err)
            case Success(_) => Failure(new Exception("unexpected success"))
          }
          _ = info(s"client creation with wrong token fails with $err")
          goodToken = getToken(userId)
          clients <- scriptClients(token = goodToken)
          _ = info(s"client creation with valid token succeeds")
          _ <- run(
            clients,
            Ref.QualifiedName.assertFromString("ScriptTest:auth"),
            inputValue = Some(
              Value.ValueRecord(
                None,
                ImmArray(
                  None -> Value.ValueParty(Ref.Party.assertFromString(parties.head)),
                  None -> Value.ValueParty(Ref.Party.assertFromString(parties.tail.head)),
                ),
              )
            ),
            dar = stableDar,
          )
        } yield succeed
      }
    }
  }
}
