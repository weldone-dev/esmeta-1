// package esmeta.web.routes

// import akka.http.scaladsl.model.*
// import akka.http.scaladsl.server.Directives.*
// import akka.http.scaladsl.server.Route
// import esmeta.cfg.CFG
// import esmeta.web.*
// import io.circe.*, io.circe.syntax.*, io.circe.parser.*
// import io.circe.generic.semiauto.*

// // exec router
// object ExecRoute {

//   /** conversion for HTTP response */
//   given Conversion[Debugger#StepResult, String] = _.ordinal.toString

//   // root router
//   def apply(cfg: CFG): Route = {
//     post {
//       concat(
//         path("run") {
//           entity(as[String]) { raw =>
//             decode[(String, List[(Boolean, Int, List[Int], Boolean)])](
//               raw,
//             ) match
//               case Left(err) => ??? // TODO handle error
//               case Right((sourceText, bpDatas)) =>
//                 initDebugger(cfg, sourceText)
//                 for { data <- bpDatas } debugger.addBreak(data)
//             complete(HttpEntity(ContentTypes.`application/json`, "null"))
//           }
//         },
//         // spec step
//         path("specStep") {
//           complete(
//             HttpEntity(ContentTypes.`application/json`, debugger.specStep),
//           )
//         },
//         // spec step-over
//         path("specStepOver") {
//           complete(
//             HttpEntity(ContentTypes.`application/json`, debugger.specStepOver),
//           )
//         },
//         // spec step-out
//         path("specStepOut") {
//           complete(
//             HttpEntity(ContentTypes.`application/json`, debugger.specStepOut),
//           )
//         },
//         // spec continue
//         path("specContinue") {
//           complete(
//             HttpEntity(ContentTypes.`application/json`, debugger.continue),
//           )
//         },
//         // ECMAScript steps
//         path("esStep") {
//           complete(HttpEntity(ContentTypes.`application/json`, debugger.esStep))
//         },
//         // ECMAScript step-over
//         path("esStepOver") {
//           complete(
//             HttpEntity(ContentTypes.`application/json`, debugger.esStepOver),
//           )
//         },
//         //  step-out
//         path("esStepOut") {
//           complete(
//             HttpEntity(ContentTypes.`application/json`, debugger.esStepOut),
//           )
//         },
//       )
//     }
//   }
// }
