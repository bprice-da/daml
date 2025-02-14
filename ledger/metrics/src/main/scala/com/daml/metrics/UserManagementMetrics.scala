// Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.metrics

import com.daml.metrics.api.MetricHandle.LabeledMetricsFactory
import com.daml.metrics.api.MetricName

class UserManagementMetrics(
    prefix: MetricName,
    labeledFactory: LabeledMetricsFactory,
) extends DatabaseMetricsFactory(prefix, labeledFactory) {

  val cache = new CacheMetrics(prefix :+ "cache", labeledFactory)

  val getUserInfo: DatabaseMetrics = createDbMetrics("get_user_info")
  val createUser: DatabaseMetrics = createDbMetrics("create_user")
  val deleteUser: DatabaseMetrics = createDbMetrics("delete_user")
  val updateUser: DatabaseMetrics = createDbMetrics("update_user")
  val grantRights: DatabaseMetrics = createDbMetrics("grant_rights")
  val revokeRights: DatabaseMetrics = createDbMetrics("revoke_rights")
  val listUsers: DatabaseMetrics = createDbMetrics("list_users")

}
