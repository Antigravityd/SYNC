# Generated from BQNHDL.g4 by ANTLR 4.5.3
from antlr4 import *
if __name__ is not None and "." in __name__:
    from .BQNHDLParser import BQNHDLParser
else:
    from BQNHDLParser import BQNHDLParser

# This class defines a complete listener for a parse tree produced by BQNHDLParser.
class BQNHDLListener(ParseTreeListener):

    # Enter a parse tree produced by BQNHDLParser#program.
    def enterProgram(self, ctx:BQNHDLParser.ProgramContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#program.
    def exitProgram(self, ctx:BQNHDLParser.ProgramContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#stmt.
    def enterStmt(self, ctx:BQNHDLParser.StmtContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#stmt.
    def exitStmt(self, ctx:BQNHDLParser.StmtContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#sep.
    def enterSep(self, ctx:BQNHDLParser.SepContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#sep.
    def exitSep(self, ctx:BQNHDLParser.SepContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#expr.
    def enterExpr(self, ctx:BQNHDLParser.ExprContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#expr.
    def exitExpr(self, ctx:BQNHDLParser.ExprContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#export.
    def enterExport(self, ctx:BQNHDLParser.ExportContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#export.
    def exitExport(self, ctx:BQNHDLParser.ExportContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#anything.
    def enterAnything(self, ctx:BQNHDLParser.AnythingContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#anything.
    def exitAnything(self, ctx:BQNHDLParser.AnythingContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#mod2.
    def enterMod2(self, ctx:BQNHDLParser.Mod2Context):
        pass

    # Exit a parse tree produced by BQNHDLParser#mod2.
    def exitMod2(self, ctx:BQNHDLParser.Mod2Context):
        pass


    # Enter a parse tree produced by BQNHDLParser#mod1.
    def enterMod1(self, ctx:BQNHDLParser.Mod1Context):
        pass

    # Exit a parse tree produced by BQNHDLParser#mod1.
    def exitMod1(self, ctx:BQNHDLParser.Mod1Context):
        pass


    # Enter a parse tree produced by BQNHDLParser#func.
    def enterFunc(self, ctx:BQNHDLParser.FuncContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#func.
    def exitFunc(self, ctx:BQNHDLParser.FuncContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#atom.
    def enterAtom(self, ctx:BQNHDLParser.AtomContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#atom.
    def exitAtom(self, ctx:BQNHDLParser.AtomContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#lis.
    def enterLis(self, ctx:BQNHDLParser.LisContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#lis.
    def exitLis(self, ctx:BQNHDLParser.LisContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#subject.
    def enterSubject(self, ctx:BQNHDLParser.SubjectContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#subject.
    def exitSubject(self, ctx:BQNHDLParser.SubjectContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#asgn.
    def enterAsgn(self, ctx:BQNHDLParser.AsgnContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#asgn.
    def exitAsgn(self, ctx:BQNHDLParser.AsgnContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#m2Expr.
    def enterM2Expr(self, ctx:BQNHDLParser.M2ExprContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#m2Expr.
    def exitM2Expr(self, ctx:BQNHDLParser.M2ExprContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#m1Expr.
    def enterM1Expr(self, ctx:BQNHDLParser.M1ExprContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#m1Expr.
    def exitM1Expr(self, ctx:BQNHDLParser.M1ExprContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#derv.
    def enterDerv(self, ctx:BQNHDLParser.DervContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#derv.
    def exitDerv(self, ctx:BQNHDLParser.DervContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#ret.
    def enterRet(self, ctx:BQNHDLParser.RetContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#ret.
    def exitRet(self, ctx:BQNHDLParser.RetContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#operand.
    def enterOperand(self, ctx:BQNHDLParser.OperandContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#operand.
    def exitOperand(self, ctx:BQNHDLParser.OperandContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#fork.
    def enterFork(self, ctx:BQNHDLParser.ForkContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#fork.
    def exitFork(self, ctx:BQNHDLParser.ForkContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#train.
    def enterTrain(self, ctx:BQNHDLParser.TrainContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#train.
    def exitTrain(self, ctx:BQNHDLParser.TrainContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#funcExpr.
    def enterFuncExpr(self, ctx:BQNHDLParser.FuncExprContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#funcExpr.
    def exitFuncExpr(self, ctx:BQNHDLParser.FuncExprContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#arg.
    def enterArg(self, ctx:BQNHDLParser.ArgContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#arg.
    def exitArg(self, ctx:BQNHDLParser.ArgContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#nothing.
    def enterNothing(self, ctx:BQNHDLParser.NothingContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#nothing.
    def exitNothing(self, ctx:BQNHDLParser.NothingContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#name.
    def enterName(self, ctx:BQNHDLParser.NameContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#name.
    def exitName(self, ctx:BQNHDLParser.NameContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#lhs_any.
    def enterLhs_any(self, ctx:BQNHDLParser.Lhs_anyContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#lhs_any.
    def exitLhs_any(self, ctx:BQNHDLParser.Lhs_anyContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#lhs_atom.
    def enterLhs_atom(self, ctx:BQNHDLParser.Lhs_atomContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#lhs_atom.
    def exitLhs_atom(self, ctx:BQNHDLParser.Lhs_atomContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#lhs_elt.
    def enterLhs_elt(self, ctx:BQNHDLParser.Lhs_eltContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#lhs_elt.
    def exitLhs_elt(self, ctx:BQNHDLParser.Lhs_eltContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#lhs_entry.
    def enterLhs_entry(self, ctx:BQNHDLParser.Lhs_entryContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#lhs_entry.
    def exitLhs_entry(self, ctx:BQNHDLParser.Lhs_entryContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#lhsStr.
    def enterLhsStr(self, ctx:BQNHDLParser.LhsStrContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#lhsStr.
    def exitLhsStr(self, ctx:BQNHDLParser.LhsStrContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#lhsList.
    def enterLhsList(self, ctx:BQNHDLParser.LhsListContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#lhsList.
    def exitLhsList(self, ctx:BQNHDLParser.LhsListContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#lhs.
    def enterLhs(self, ctx:BQNHDLParser.LhsContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#lhs.
    def exitLhs(self, ctx:BQNHDLParser.LhsContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#subExpr.
    def enterSubExpr(self, ctx:BQNHDLParser.SubExprContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#subExpr.
    def exitSubExpr(self, ctx:BQNHDLParser.SubExprContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#headW.
    def enterHeadW(self, ctx:BQNHDLParser.HeadWContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#headW.
    def exitHeadW(self, ctx:BQNHDLParser.HeadWContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#headX.
    def enterHeadX(self, ctx:BQNHDLParser.HeadXContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#headX.
    def exitHeadX(self, ctx:BQNHDLParser.HeadXContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#headF.
    def enterHeadF(self, ctx:BQNHDLParser.HeadFContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#headF.
    def exitHeadF(self, ctx:BQNHDLParser.HeadFContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#headG.
    def enterHeadG(self, ctx:BQNHDLParser.HeadGContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#headG.
    def exitHeadG(self, ctx:BQNHDLParser.HeadGContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#mod1H1.
    def enterMod1H1(self, ctx:BQNHDLParser.Mod1H1Context):
        pass

    # Exit a parse tree produced by BQNHDLParser#mod1H1.
    def exitMod1H1(self, ctx:BQNHDLParser.Mod1H1Context):
        pass


    # Enter a parse tree produced by BQNHDLParser#mod2H1.
    def enterMod2H1(self, ctx:BQNHDLParser.Mod2H1Context):
        pass

    # Exit a parse tree produced by BQNHDLParser#mod2H1.
    def exitMod2H1(self, ctx:BQNHDLParser.Mod2H1Context):
        pass


    # Enter a parse tree produced by BQNHDLParser#funcHead.
    def enterFuncHead(self, ctx:BQNHDLParser.FuncHeadContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#funcHead.
    def exitFuncHead(self, ctx:BQNHDLParser.FuncHeadContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#m1Head.
    def enterM1Head(self, ctx:BQNHDLParser.M1HeadContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#m1Head.
    def exitM1Head(self, ctx:BQNHDLParser.M1HeadContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#m2Head.
    def enterM2Head(self, ctx:BQNHDLParser.M2HeadContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#m2Head.
    def exitM2Head(self, ctx:BQNHDLParser.M2HeadContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#undoHead.
    def enterUndoHead(self, ctx:BQNHDLParser.UndoHeadContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#undoHead.
    def exitUndoHead(self, ctx:BQNHDLParser.UndoHeadContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#body.
    def enterBody(self, ctx:BQNHDLParser.BodyContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#body.
    def exitBody(self, ctx:BQNHDLParser.BodyContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#fCase.
    def enterFCase(self, ctx:BQNHDLParser.FCaseContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#fCase.
    def exitFCase(self, ctx:BQNHDLParser.FCaseContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#mCase.
    def enterMCase(self, ctx:BQNHDLParser.MCaseContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#mCase.
    def exitMCase(self, ctx:BQNHDLParser.MCaseContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#cCase.
    def enterCCase(self, ctx:BQNHDLParser.CCaseContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#cCase.
    def exitCCase(self, ctx:BQNHDLParser.CCaseContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#fMain.
    def enterFMain(self, ctx:BQNHDLParser.FMainContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#fMain.
    def exitFMain(self, ctx:BQNHDLParser.FMainContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#mMain.
    def enterMMain(self, ctx:BQNHDLParser.MMainContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#mMain.
    def exitMMain(self, ctx:BQNHDLParser.MMainContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#cMain.
    def enterCMain(self, ctx:BQNHDLParser.CMainContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#cMain.
    def exitCMain(self, ctx:BQNHDLParser.CMainContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#brSub.
    def enterBrSub(self, ctx:BQNHDLParser.BrSubContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#brSub.
    def exitBrSub(self, ctx:BQNHDLParser.BrSubContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#brFunc.
    def enterBrFunc(self, ctx:BQNHDLParser.BrFuncContext):
        pass

    # Exit a parse tree produced by BQNHDLParser#brFunc.
    def exitBrFunc(self, ctx:BQNHDLParser.BrFuncContext):
        pass


    # Enter a parse tree produced by BQNHDLParser#brMod1.
    def enterBrMod1(self, ctx:BQNHDLParser.BrMod1Context):
        pass

    # Exit a parse tree produced by BQNHDLParser#brMod1.
    def exitBrMod1(self, ctx:BQNHDLParser.BrMod1Context):
        pass


    # Enter a parse tree produced by BQNHDLParser#brMod2.
    def enterBrMod2(self, ctx:BQNHDLParser.BrMod2Context):
        pass

    # Exit a parse tree produced by BQNHDLParser#brMod2.
    def exitBrMod2(self, ctx:BQNHDLParser.BrMod2Context):
        pass


