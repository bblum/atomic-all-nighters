data CTranslUnit = CTranslUnit [CExtDecl] NodeInfo
data CExtDecl
    = CDeclExt CDecl
    | CFDefExt CFunDef
    | CAsmExt CStrLit 
data CFunDef = CFunDef [CDeclSpec] CDeclr [CDecl] CStat NodeInfo
data CDecl = CDecl [CDeclSpec] [(Maybe CDeclr, Maybe CInit, Maybe CExpr)] NodeInfo
data CStructUnion = CStruct CStructTag (Maybe Ident) (Maybe [CDecl]) [CAttr] NodeInfo
data CStructTag
    = CStructTag
    | CUnionTag 
data CEnum = CEnum (Maybe Ident) (Maybe [(Ident, Maybe CExpr)]) [CAttr] NodeInfo
data CDeclSpec
    = CStorageSpec CStorageSpec
    | CTypeSpec CTypeSpec
    | CTypeQual CTypeQual 
partitionDeclSpecs :: [CDeclSpec] -> ([CStorageSpec], [CAttr], [CTypeQual], [CTypeSpec], Bool)
data CStorageSpec
    = CAuto NodeInfo
    | CRegister NodeInfo
    | CStatic NodeInfo
    | CExtern NodeInfo
    | CTypedef NodeInfo
    | CThread NodeInfo 
data CTypeSpec
    = CVoidType NodeInfo
    | CCharType NodeInfo
    | CShortType NodeInfo
    | CIntType NodeInfo
    | CLongType NodeInfo
    | CFloatType NodeInfo
    | CDoubleType NodeInfo
    | CSignedType NodeInfo
    | CUnsigType NodeInfo
    | CBoolType NodeInfo
    | CComplexType NodeInfo
    | CSUType CStructUnion NodeInfo
    | CEnumType CEnum NodeInfo
    | CTypeDef Ident NodeInfo
    | CTypeOfExpr CExpr NodeInfo
    | CTypeOfType CDecl NodeInfo 
isSUEDef :: CTypeSpec -> Bool
data CTypeQual
    = CConstQual NodeInfo
    | CVolatQual NodeInfo
    | CRestrQual NodeInfo
    | CInlineQual NodeInfo
    | CAttrQual CAttr 
data CAttr = CAttr Ident [CExpr] NodeInfo
data CDeclr = CDeclr (Maybe Ident) [CDerivedDeclr] (Maybe CStrLit) [CAttr] NodeInfo
data CDerivedDeclr
    = CPtrDeclr [CTypeQual] NodeInfo
    | CArrDeclr [CTypeQual] CArrSize NodeInfo
    | CFunDeclr (Either [Ident] ([CDecl], Bool)) [CAttr] NodeInfo 
data CArrSize
    = CNoArrSize Bool
    | CArrSize Bool CExpr 
data CInit
    = CInitExpr CExpr NodeInfo
    | CInitList CInitList NodeInfo 
type CInitList = [([CDesignator], CInit)]
data CDesignator
    = CArrDesig CExpr NodeInfo
    | CMemberDesig Ident NodeInfo
    | CRangeDesig CExpr CExpr NodeInfo 
data CStat
    = CLabel Ident CStat [CAttr] NodeInfo
    | CCase CExpr CStat NodeInfo
    | CCases CExpr CExpr CStat NodeInfo
    | CDefault CStat NodeInfo
    | CExpr (Maybe CExpr) NodeInfo
    | CCompound [Ident] [CBlockItem] NodeInfo
    | CIf CExpr CStat (Maybe CStat) NodeInfo
    | CSwitch CExpr CStat NodeInfo
    | CWhile CExpr CStat Bool NodeInfo
    | CFor (Either (Maybe CExpr) CDecl) (Maybe CExpr) (Maybe CExpr) CStat NodeInfo
    | CGoto Ident NodeInfo
    | CGotoPtr CExpr NodeInfo
    | CCont NodeInfo
    | CBreak NodeInfo
    | CReturn (Maybe CExpr) NodeInfo
    | CAsm CAsmStmt NodeInfo 
data CBlockItem
    = CBlockStmt CStat
    | CBlockDecl CDecl
    | CNestedFunDef CFunDef 
data CAsmStmt = CAsmStmt (Maybe CTypeQual) CStrLit [CAsmOperand] [CAsmOperand] [CStrLit] NodeInfo
data CAsmOperand = CAsmOperand (Maybe Ident) CStrLit CExpr NodeInfo
data CExpr
    = CComma [CExpr] NodeInfo
    | CAssign CAssignOp CExpr CExpr NodeInfo
    | CCond CExpr (Maybe CExpr) CExpr NodeInfo
    | CBinary CBinaryOp CExpr CExpr NodeInfo
    | CCast CDecl CExpr NodeInfo
    | CUnary CUnaryOp CExpr NodeInfo
    | CSizeofExpr CExpr NodeInfo
    | CSizeofType CDecl NodeInfo
    | CAlignofExpr CExpr NodeInfo
    | CAlignofType CDecl NodeInfo
    | CComplexReal CExpr NodeInfo
    | CComplexImag CExpr NodeInfo
    | CIndex CExpr CExpr NodeInfo
    | CCall CExpr [CExpr] NodeInfo
    | CMember CExpr Ident Bool NodeInfo
    | CVar Ident NodeInfo
    | CConst CConst
    | CCompoundLit CDecl CInitList NodeInfo
    | CStatExpr CStat NodeInfo
    | CLabAddrExpr Ident NodeInfo
    | CBuiltinExpr CBuiltin 
data CAssignOp
    = CAssignOp
    | CMulAssOp
    | CDivAssOp
    | CRmdAssOp
    | CAddAssOp
    | CSubAssOp
    | CShlAssOp
    | CShrAssOp
    | CAndAssOp
    | CXorAssOp
    | COrAssOp 
data CBinaryOp
    = CMulOp
    | CDivOp
    | CRmdOp
    | CAddOp
    | CSubOp
    | CShlOp
    | CShrOp
    | CLeOp
    | CGrOp
    | CLeqOp
    | CGeqOp
    | CEqOp
    | CNeqOp
    | CAndOp
    | CXorOp
    | COrOp
    | CLndOp
    | CLorOp 
data CUnaryOp
    = CPreIncOp
    | CPreDecOp
    | CPostIncOp
    | CPostDecOp
    | CAdrOp
    | CIndOp
    | CPlusOp
    | CMinOp
    | CCompOp
    | CNegOp 
data CBuiltin
    = CBuiltinVaArg CExpr CDecl NodeInfo
    | CBuiltinOffsetOf CDecl [CDesignator] NodeInfo
    | CBuiltinTypesCompatible CDecl CDecl NodeInfo 
data CConst
    = CIntConst CInteger NodeInfo
    | CCharConst CChar NodeInfo
    | CFloatConst CFloat NodeInfo
    | CStrConst CString NodeInfo 
data CStrLit = CStrLit CString NodeInfo
cstringOfLit :: CStrLit -> CString
liftStrLit :: CStrLit -> CConst
