import langoustine.lsp.app.LangoustineApp

import cats.effect.*
import jsonrpclib.fs2.*

import fs2.io.file.Files
import fs2.text
import fs2.io.file.Path
import cats.syntax.all.*
import org.yaml.snakeyaml.error.Mark
import langoustine.lsp.LSPBuilder
import java.io.File
import langoustine.lsp.Communicate

object LSP extends LangoustineApp.Simple:
  override def server: IO[LSPBuilder[cats.effect.IO]] =
    IO.pure(GithubActionsLSP)

def GithubActionsLSP =
  import langoustine.lsp.all.*
  extension (m: Mark)
    def pos =
      Position(
        line = m.getLine(),
        character = m.getColumn()
      )

  extension (n: org.yaml.snakeyaml.nodes.Node)
    def range =
      Range(n.getStartMark().pos, n.getEndMark().pos)

  def processFile(file: File) =
    IO {
      processWorkflowFile(file)
    }
  def processUri(uri: DocumentUri, back: Communicate[IO]) =
    val path = uri.value.drop("file://".length)

    val documentBeginning = Range(Position.documentBeginning, Position.documentBeginning)

    processFile(new File(path)).void.attempt.flatMap {
      case Left(Err(msg, where)) =>
        val diag = Diagnostic(
          range = where.map(_.range).getOrElse(documentBeginning),
          message = msg,
          severity = Opt(DiagnosticSeverity.Error)
        )

        back.notification(
          textDocument.publishDiagnostics,
          PublishDiagnosticsParams(uri, diagnostics = Vector(diag))
        )
      case Right(()) =>
        back.notification(
          textDocument.publishDiagnostics,
          PublishDiagnosticsParams(uri, diagnostics = Vector.empty)
        )
      case Left(err) =>
        val diag = Diagnostic(
          range = documentBeginning,
          message = err.getMessage,
          severity = Opt(DiagnosticSeverity.Error)
        )

        back.notification(
          textDocument.publishDiagnostics,
          PublishDiagnosticsParams(uri, diagnostics = Vector(diag))
        )
    }
  end processUri

  val C = cats.effect.std.Console[IO]

  LSPBuilder
    .create[IO]
    .handleRequest(initialize) { in =>
      in.toClient.notification(
        window.showMessage,
        ShowMessageParams(
          message = "Hello from GHA LSP",
          `type` = MessageType.Info
        )
      ) *>
        IO {
          InitializeResult(
            ServerCapabilities(
              hoverProvider = Opt(true),
              definitionProvider = Opt(true),
              documentSymbolProvider = Opt(true),
              renameProvider = Opt(true),
              textDocumentSync = Opt(
                TextDocumentSyncOptions(
                  openClose = Opt(true),
                  save = Opt(true)
                )
              )
            ),
            Opt(
              InitializeResult
                .ServerInfo(name = "Quickmaffs LSP", version = Opt("0.0.1"))
            )
          )
        }
    }
    .handleNotification(textDocument.didOpen) { in =>
      processUri(in.params.textDocument.uri, in.toClient)
    }
    .handleNotification(textDocument.didSave) { in =>
      processUri(in.params.textDocument.uri, in.toClient)
    }
end GithubActionsLSP
