using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;
using System.Threading.Tasks;
using JetBrains.Annotations;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Operations;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using VerifyCS = Microsoft.CodeAnalysis.CSharp.Testing.MSTest.CodeFixVerifier<
    IssueRepro.AnalyzerTest, IssueRepro.CodeFixTest>;

namespace IssueRepro
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    [UsedImplicitly]
    public class AnalyzerTest : DiagnosticAnalyzer
    {
        public static readonly DiagnosticDescriptor Diag = new(
            "DIAG01", "Foo", "Bar", "Baz", DiagnosticSeverity.Error, true);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } =
            ImmutableArray.Create(Diag);

        public override void Initialize(AnalysisContext context)
        {
            context.EnableConcurrentExecution();
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.RegisterOperationAction(AnalyzeOp, OperationKind.Binary);
        }

        private void AnalyzeOp(OperationAnalysisContext context)
        {
            var binOp = (IBinaryOperation)context.Operation;
            if (binOp.OperatorKind is BinaryOperatorKind.Equals)
            {
                context.ReportDiagnostic(Diagnostic.Create(Diag, binOp.Syntax.GetLocation()));
            }
        }
    }

    public class CodeFixTest : CodeFixProvider
    {
        public override ImmutableArray<string> FixableDiagnosticIds { get; } =
            ImmutableArray.Create(AnalyzerTest.Diag.Id);

        public override Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            foreach (var diagnostic in context.Diagnostics)
            {
                context.RegisterCodeFix(CodeAction.Create("Fix now!",
                        ct => ApplyFix(context.Document, diagnostic, ct),
                        diagnostic.Location.ToString()),
                    diagnostic);
            }

            return Task.CompletedTask;
        }

        private static async Task<Document> ApplyFix(Document document, Diagnostic diagnostic, CancellationToken ct)
        {
            var syntaxRoot = await document.GetSyntaxRootAsync(ct).ConfigureAwait(false);
            var binaryOp = (BinaryExpressionSyntax)syntaxRoot!.FindNode(diagnostic.Location.SourceSpan, false, true);
            var nodesToRewrite = new HashSet<BinaryExpressionSyntax>();
            nodesToRewrite.Add(binaryOp);

            var rewriter = new Rewriter(nodesToRewrite);
            var newSyntaxRoot = rewriter.Visit(syntaxRoot);
            return document.WithSyntaxRoot(newSyntaxRoot);
        }
        
        private sealed class Rewriter : CSharpSyntaxRewriter
        {
            private readonly HashSet<BinaryExpressionSyntax> _nodesToRewrite;

            public Rewriter(HashSet<BinaryExpressionSyntax> nodesToRewrite)
            {
                _nodesToRewrite = nodesToRewrite;
            }

            public override SyntaxNode VisitBinaryExpression(BinaryExpressionSyntax node)
            {
                var result = (BinaryExpressionSyntax) base.VisitBinaryExpression(node)!;
                var nodeKind = node.Kind();
                if (nodeKind is not (SyntaxKind.EqualsExpression or SyntaxKind.NotEqualsExpression) ||
                    !_nodesToRewrite.Contains(node)) {
                    return result;
                }

                ExpressionSyntax value;
                if (result.Left.IsKind(SyntaxKind.NullLiteralExpression)) {
                    value = result.Right;
                }
                else if (result.Right.IsKind(SyntaxKind.NullLiteralExpression)) {
                    value = result.Left;
                }
                else {
                    return result;
                }

                PatternSyntax matchedPattern = SyntaxFactory.ConstantPattern(
                    SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression)
                );
                var isNegated = nodeKind is SyntaxKind.NotEqualsExpression;
                if (isNegated) {
                    matchedPattern = SyntaxFactory.UnaryPattern(SyntaxFactory.Token(SyntaxKind.NotKeyword), matchedPattern);
                }

                var isExpression = SyntaxFactory.IsPatternExpression(
                        value.WithoutTrivia(),
                        matchedPattern)
                    .WithTrailingTrivia(SyntaxFactory.Whitespace("        "));
                return isExpression;
            }
        }
    }

    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public async Task TestMethod1()
        {
            const string original = @"
public class C {
    public static void M(C c) {
        if ({|#0:c == null|}    ) { }
    }
}";
            const string fixedCode = @"
public class C {
    public static void M(C c) {
        if (c is null    ) { }
    }
}";
            await VerifyCS.VerifyCodeFixAsync(original,
                VerifyCS.Diagnostic(AnalyzerTest.Diag).WithLocation(0),
                fixedCode);
        }
    }
}