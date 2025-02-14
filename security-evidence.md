# Security tests, by category

## Authorization:
- auth and auth-* should not be set together for the trigger service: [CliConfigTest.scala](triggers/service/src/test-suite/scala/com/daml/lf/engine/trigger/CliConfigTest.scala#L40)
- badly-authorized create is rejected: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L59)
- badly-authorized exercise is rejected: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L159)
- badly-authorized exercise/create (create is unauthorized) is rejected: [AuthPropagationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthPropagationSpec.scala#L271)
- badly-authorized exercise/create (exercise is unauthorized) is rejected: [AuthPropagationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthPropagationSpec.scala#L239)
- badly-authorized exercise/exercise (no implicit authority from outer exercise) is rejected: [AuthPropagationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthPropagationSpec.scala#L330)
- badly-authorized fetch is rejected: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L94)
- badly-authorized lookup is rejected: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L117)
- create with no signatories is rejected: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L49)
- create with non-signatory maintainers is rejected: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L71)
- error on specifying both authCommonUri and authInternalUri/authExternalUri for the trigger service: [AuthorizationConfigTest.scala](triggers/service/src/test-suite/scala/com/daml/lf/engine/trigger/AuthorizationConfigTest.scala#L24)
- error on specifying only authInternalUri and no authExternalUri for the trigger service: [AuthorizationConfigTest.scala](triggers/service/src/test-suite/scala/com/daml/lf/engine/trigger/AuthorizationConfigTest.scala#L52)
- exercise with no controllers is rejected: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L149)
- well-authorized create is accepted: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L42)
- well-authorized exercise is accepted: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L142)
- well-authorized exercise/create is accepted: [AuthPropagationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthPropagationSpec.scala#L217)
- well-authorized exercise/exercise is accepted: [AuthPropagationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthPropagationSpec.scala#L373)
- well-authorized fetch is accepted: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L88)
- well-authorized lookup is accepted: [AuthorizationSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/AuthorizationSpec.scala#L110)

## Availability:
- Tail call optimization: Tail recursion does not blow the scala JVM stack.: [TailCallTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/TailCallTest.scala#L16)

## Confidentiality:
- ensure correct privacy for create node: [BlindingSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/BlindingSpec.scala#L32)
- ensure correct privacy for exercise node (consuming): [BlindingSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/BlindingSpec.scala#L43)
- ensure correct privacy for exercise node (non-consuming): [BlindingSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/BlindingSpec.scala#L62)
- ensure correct privacy for exercise subtree: [BlindingSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/BlindingSpec.scala#L137)
- ensure correct privacy for fetch node: [BlindingSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/BlindingSpec.scala#L82)
- ensure correct privacy for lookup-by-key node (found): [BlindingSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/BlindingSpec.scala#L94)
- ensure correct privacy for lookup-by-key node (not-found): [BlindingSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/BlindingSpec.scala#L115)
- ensure correct privacy for rollback subtree: [BlindingSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/BlindingSpec.scala#L199)

## Integrity:
- Evaluation order of create with authorization failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L560)
- Evaluation order of create with contract ID in contract key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L583)
- Evaluation order of create with contract key exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L633)
- Evaluation order of create with create argument exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L610)
- Evaluation order of create with duplicate contract key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L511)
- Evaluation order of create with empty contract key maintainers: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L536)
- Evaluation order of create with failed precondition: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L493)
- Evaluation order of create_interface with authorization failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L754)
- Evaluation order of create_interface with contract ID in contract key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L777)
- Evaluation order of create_interface with contract key exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L827)
- Evaluation order of create_interface with create argument exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L804)
- Evaluation order of create_interface with duplicate contract key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L703)
- Evaluation order of create_interface with empty contract key maintainers: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L728)
- Evaluation order of create_interface with failed precondition: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L685)
- Evaluation order of exercise by interface of a cached global contract that does not implement the interface.: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1757)
- Evaluation order of exercise by interface of an inactive global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1739)
- Evaluation order of exercise by interface of cached global contract with failed authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1797)
- Evaluation order of exercise of a cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1130)
- Evaluation order of exercise of a non-cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L897)
- Evaluation order of exercise of a non-cached global contract with inconsistent key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L922)
- Evaluation order of exercise of a wrongly typed cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L994)
- Evaluation order of exercise of a wrongly typed non-cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L882)
- Evaluation order of exercise of an inactive global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L977)
- Evaluation order of exercise of an inactive local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1072)
- Evaluation order of exercise of an unknown contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1154)
- Evaluation order of exercise of an wrongly typed local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1091)
- Evaluation order of exercise of cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1030)
- Evaluation order of exercise with argument exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1168)
- Evaluation order of exercise with output exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1192)
- Evaluation order of exercise-by-key of a cached global contract with visibility failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1409)
- Evaluation order of exercise-by-key of a non-cached global contract with visibility failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1291)
- Evaluation order of exercise_by_key of a cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1476)
- Evaluation order of exercise_by_key of a local contract with visibility failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1499)
- Evaluation order of exercise_by_key of a non-cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1265)
- Evaluation order of exercise_by_key of a wrongly typed cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1366)
- Evaluation order of exercise_by_key of an inactive global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1347)
- Evaluation order of exercise_by_key of an inactive local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1456)
- Evaluation order of exercise_by_key of an unknown contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1533)
- Evaluation order of exercise_by_key of cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1385)
- Evaluation order of exercise_by_key with argument exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1548)
- Evaluation order of exercise_by_key with contract ID in contract key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1613)
- Evaluation order of exercise_by_key with result exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1573)
- Evaluation order of exercise_interface of a cached local contract with failed authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1909)
- Evaluation order of exercise_interface of a non-cached global contract with failed authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1684)
- Evaluation order of exercise_interface of an inactive local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1847)
- Evaluation order of exercise_interface of an local contract not implementing the interface: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1866)
- Evaluation order of exercise_vy_key with empty contract key maintainers: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1599)
- Evaluation order of fetch of a cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2194)
- Evaluation order of fetch of a non-cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1987)
- Evaluation order of fetch of a non-cached global contract with inconsistent key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2010)
- Evaluation order of fetch of a wrongly typed cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2073)
- Evaluation order of fetch of a wrongly typed disclosed contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2214)
- Evaluation order of fetch of a wrongly typed non-cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1972)
- Evaluation order of fetch of an inactive global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2057)
- Evaluation order of fetch of an inactive local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2142)
- Evaluation order of fetch of an unknown contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2233)
- Evaluation order of fetch of an wrongly typed local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2159)
- Evaluation order of fetch of cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2107)
- Evaluation order of fetch-by-key of a cached global contract with visibility failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2404)
- Evaluation order of fetch-by-key of a non-cached global contract with visibility failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2320)
- Evaluation order of fetch_by_key of a cached global contract with authorization failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2387)
- Evaluation order of fetch_by_key of a local contract with authorization failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2463)
- Evaluation order of fetch_by_key of a non-cached global contract with authorization failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2297)
- Evaluation order of fetch_by_key of an inactive global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2368)
- Evaluation order of fetch_by_key of an inactive global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2445)
- Evaluation order of fetch_by_key of an unknown contract key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2492)
- Evaluation order of fetch_by_key with contract ID in contract key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2522)
- Evaluation order of fetch_by_key with contract key exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2536)
- Evaluation order of fetch_by_key with empty contract key maintainers: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2508)
- Evaluation order of fetch_interface of a cached global contract not implementing the interface.: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2656)
- Evaluation order of fetch_interface of a cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2780)
- Evaluation order of fetch_interface of a non-cached global contract that doesn't implement interface.: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2578)
- Evaluation order of fetch_interface of a non-cached global contract with failed authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2595)
- Evaluation order of fetch_interface of an inactive global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2639)
- Evaluation order of fetch_interface of an inactive local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2727)
- Evaluation order of fetch_interface of an local contract not implementing the interface: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2744)
- Evaluation order of fetch_interface of an unknown contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2798)
- Evaluation order of fetch_interface of cached global contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2692)
- Evaluation order of lookup of a cached global contract with visibility failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2948)
- Evaluation order of lookup of a non-cached global contract with visibility failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2865)
- Evaluation order of lookup_by_key of a cached global contract with authorization failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2931)
- Evaluation order of lookup_by_key of a local contract with authorization failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L3022)
- Evaluation order of lookup_by_key of a local contract with failure authorization: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L3006)
- Evaluation order of lookup_by_key of a non-cached global contract with authorization failure: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2842)
- Evaluation order of lookup_by_key of an inactive global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2913)
- Evaluation order of lookup_by_key of an inactive local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2989)
- Evaluation order of lookup_by_key of an unknown contract key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L3052)
- Evaluation order of lookup_by_key with contract ID in contract key: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L3082)
- Evaluation order of lookup_by_key with contract key exceeding max nesting: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L3096)
- Evaluation order of lookup_by_key with empty contract key maintainers: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L3068)
- Evaluation order of successful create: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L469)
- Evaluation order of successful create_interface: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L660)
- Evaluation order of successful exercise by interface of a non-cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1638)
- Evaluation order of successful exercise of a cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L954)
- Evaluation order of successful exercise of a local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1050)
- Evaluation order of successful exercise of a non-cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L856)
- Evaluation order of successful exercise_by_key of a cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1322)
- Evaluation order of successful exercise_by_key of a local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1433)
- Evaluation order of successful exercise_by_key of a non-cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1222)
- Evaluation order of successful exercise_interface of a cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1714)
- Evaluation order of successful exercise_interface of a local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1823)
- Evaluation order of successful fetch of a cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2040)
- Evaluation order of successful fetch of a local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2127)
- Evaluation order of successful fetch of a non-cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1949)
- Evaluation order of successful fetch_by_key of a cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2351)
- Evaluation order of successful fetch_by_key of a local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2429)
- Evaluation order of successful fetch_by_key of a non-cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2252)
- Evaluation order of successful fetch_interface of a cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2622)
- Evaluation order of successful fetch_interface of a local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2712)
- Evaluation order of successful fetch_interface of a non-cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2554)
- Evaluation order of successful lookup_by_key of a cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2896)
- Evaluation order of successful lookup_by_key of a local contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2973)
- Evaluation order of successful lookup_by_key of a non-cached global contract: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2818)
- Exceptions, throw/catch.: [ExceptionTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/ExceptionTest.scala#L26)
- Rollback creates cannot be exercise: [EngineTest.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/EngineTest.scala#L2181)
- This checks that type checking in exercise_interface is done after checking activeness.: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1889)
- This checks that type checking is done after checking activeness.: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1779)
- This checks that type checking is done after checking activeness.: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L2762)
- contract key behaviour (non-unique mode): [ContractKeySpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ContractKeySpec.scala#L410)
- contract key behaviour (unique mode): [ContractKeySpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ContractKeySpec.scala#L420)
- contract keys must have a non-empty set of maintainers: [ContractKeySpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ContractKeySpec.scala#L218)
- contract keys should be evaluated after ensure clause: [ContractKeySpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ContractKeySpec.scala#L185)
- contract keys should be evaluated only when executing create: [ContractKeySpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ContractKeySpec.scala#L146)
- ensure builtin operators have the correct type: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L48)
- ensure expression forms have the correct type: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L108)
- exercise-by-interface command is rejected for a: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L171)
- exercise_interface with a contract instance that does not implement the interface fails.: [EvaluationOrderTest.scala](daml-lf/interpreter/src/test/scala/com/digitalasset/daml/lf/speedy/EvaluationOrderTest.scala#L1666)
- ill-formed create API command is rejected: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L159)
- ill-formed create replay command is rejected: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L109)
- ill-formed create-and-exercise API command is rejected: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L184)
- ill-formed exception definitions are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L1867)
- ill-formed exercise API command is rejected: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L164)
- ill-formed exercise replay command is rejected: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L114)
- ill-formed exercise-by-key API command is rejected: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L175)
- ill-formed exercise-by-key replay command is rejected: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L121)
- ill-formed expressions are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L460)
- ill-formed fetch command is rejected: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L168)
- ill-formed fetch-by-key command is rejected: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L171)
- ill-formed interfaces are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L1452)
- ill-formed kinds are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L20)
- ill-formed lookup command is rejected: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L176)
- ill-formed records are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L2009)
- ill-formed templates are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L1067)
- ill-formed type synonyms applications are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L1988)
- ill-formed type synonyms definitions are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L2055)
- ill-formed types are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L100)
- ill-formed variants are rejected: [TypingSpec.scala](daml-lf/validation/src/test/scala/com/digitalasset/daml/lf/validation/TypingSpec.scala#L2032)
- well formed create API command is accepted: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L115)
- well formed create replay command is accepted: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L80)
- well formed create-and-exercise API command is accepted: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L141)
- well formed exercise API command is accepted: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L120)
- well formed exercise replay command is accepted: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L85)
- well formed exercise-by-interface command is accepted: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L134)
- well formed exercise-by-key API command is accepted: [ApiCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ApiCommandPreprocessorSpec.scala#L127)
- well formed exercise-by-key command is accepted: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L93)
- well formed fetch replay command is accepted: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L145)
- well formed fetch-by-key replay command is accepted: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L150)
- well formed lookup replay command is accepted: [ReplayCommandPreprocessorSpec.scala](daml-lf/engine/src/test/scala/com/digitalasset/daml/lf/engine/ReplayCommandPreprocessorSpec.scala#L155)


