module Interpreter where

import AbsGrammar
import Types
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe
import Data.Map
import Data.Typeable
import qualified Data.Array (bounds, (!), listArray, (//), elems, Array)
import System.IO
import Data.Functor.Identity

myPrint s = do
    liftIO $ putStr (s ++ "\n")

newLocation :: Mem -> Int
newLocation mem = do
    if Prelude.null (keys mem) then
        0
    else
        maximum (keys mem) + 1

runInterpreter tree = do
    result <- runExceptT $ runReaderT (runStateT (runProgram tree) empty) empty
    case result of
        (Left e) -> hPutStrLn stderr ("Error: " ++ e)
        (Right ret) -> return ()

runProgram :: Program -> RunnerRetType Int
runProgram (Program (topDef:program)) = do
    (newEnv, returnMain) <- runTopDef topDef
    if isNothing returnMain then
        local (const newEnv) (runProgram $ Program program)
    else
        return $ head $ maybeToList returnMain

runProgram (Program []) = do
    throwError "No main() function."

runTopDef :: TopDef -> RunnerRetType (Env, Maybe Int)
runTopDef topDef = case topDef of
    (FnDef retType ident args block) -> do
        env <- ask 
        (argsEnv, names) <- runArgs args
        mem <- get 
        let 
            location = newLocation mem
            newEnv = insert ident location env
            functionEnv = insert ident location argsEnv
        modify $ insert location (VFun $ Just $ ValidFun retType block functionEnv names)
        if ident == Ident "main" then do
            mainReturn <- local (const newEnv) (runMainExpr (EApp (Ident "main") []))
            if retType == Int then
                case mainReturn of
                    (Just (VInt i)) -> return (newEnv, Just i)
                    _ -> throwError "main() should return int."
            else
                throwError "main() should return int."
        else
            return (newEnv, Nothing)
    (VarDef (TopInit varType items)) -> do
        newEnv <- runItems varType items
        return (newEnv, Nothing)

runArgs :: [Arg] -> RunnerRetType (Env, [(Bool, Ident)])
runArgs (arg:args) = case arg of
    (Arg argType ident) -> do
        (newEnv, names) <- runArgs args
        mem <- get
        (Just zero) <- getZero argType
        let
            location = newLocation mem
        modify $ insert location zero
        return (insert ident location newEnv, (False, ident):names)
    (RefArg argType ident) -> do
        (newEnv, names) <- runArgs args
        mem <- get
        (Just zero) <- getZero argType
        let
            location = newLocation mem
        modify $ insert location zero
        return (insert ident location newEnv, (True, ident):names)

runArgs [] = do
    env <- ask
    return (env, [])

runExpr :: Expr -> RunnerRetType (Maybe VariableType)
runExpr e = do
    env <- ask 
    mem <- get
    let
        checkArray :: Ident -> RunnerRetType ()
        checkArray ident = do
            if member ident env then
                if member (env ! ident) mem then
                    return ()
                else
                    throwError "Array not initialized."
            else
                throwError $ "Array " ++ show ident ++ "does not exist."
        processExprs :: [Expr] -> RunnerRetType [VariableType]
        processExprs (expr:next) = do
            evaluatedExpr <- runExpr expr
            otherExprs <- processExprs next
            if isNothing evaluatedExpr then
                throwError "Tuple expression needs to evaluate to a value."
            else
                return $ head (maybeToList evaluatedExpr):otherExprs
        processExprs [] = do
            return []
    case e of
        (EVar ident) -> do
            if member ident env then
                return $ Just $ mem ! (env ! ident)
            else
                throwError $ "Variable " ++ show ident ++ " does not exist."
        (EArr ident expr) -> do
            evaluatedExpr <- runExpr expr
            case evaluatedExpr of
                (Just (VInt i)) -> do
                    checkArray ident
                    let 
                        getVal :: Data.Array.Array Int VariableType -> RunnerRetType (Maybe VariableType)
                        getVal arr = do
                            if i >= 0 && i <= snd (Data.Array.bounds arr) then
                                return $ Just $ arr Data.Array.! i
                            else
                                throwError "Array index not in range."
                    case mem ! (env ! ident) of
                        (VArr arrType array) -> do getVal array
                        (VTuple array) -> do getVal array
                _ -> throwError "Index must evaluate to int."
        (ESize ident) -> do
            checkArray ident
            let (VArr arrType array) = mem ! (env ! ident)
            return $ Just $ VInt (snd (Data.Array.bounds array) + 1)
        (ENewArr arrType expr) -> do
            evaluateExpr <- runExpr expr
            case evaluateExpr of
                (Just (VInt i)) -> do
                    (Just zero) <- getZero arrType
                    return $ Just (VArr arrType (Data.Array.listArray (0, i - 1) (replicate i zero)))
                _ -> throwError "Size must evaluate to int.";
        (ELitInt integer) -> do
            return $ Just $ VInt $ fromInteger integer
        ELitTrue -> do
            return $ Just $ VBool True
        ELitFalse -> do
            return $ Just $ VBool False
        (ToString expr) -> do
            evaluatedExpr <- runExpr expr
            case evaluatedExpr of
                Nothing -> throwError "Expression need to evaluate to a value."
                (Just v) -> return $ Just $ VStr (show v) 
        (ETuple exprs) -> do
            evaluatedExpressions <- processExprs exprs
            return $ Just $ VTuple (Data.Array.listArray (0, length exprs - 1) evaluatedExpressions)
        (ETuple2 expr exprs) -> runExpr (ETuple (expr:exprs))
        (NewArray exprs) -> let
                evalAllExprs :: [Expr] -> RunnerRetType [VariableType]
                evalAllExprs (expr:next) = do
                    nextEval <- evalAllExprs next
                    evaluatedExpr <- runExpr expr
                    case evaluatedExpr of
                        Nothing -> throwError "Expression needs to evaluate to a correct value."
                        (Just v) -> return (v:nextEval)

                evalAllExprs [] = return []
            in do
                evaluatedExprs <- evalAllExprs exprs
                if Prelude.null evaluatedExprs then throwError "Cannot construct empty array using []"
                else do
                    let arrType = getType mem env (head evaluatedExprs)
                    return $ Just $ VArr arrType (Data.Array.listArray (0, length evaluatedExprs - 1) evaluatedExprs)

        (EApp ident exprs) -> let
                prepareFunctionEnv :: Env -> [(Bool, Ident)] -> [Expr] -> RunnerRetType (Env, Data.Map.Map Int VariableType)
                prepareFunctionEnv functionEnv (arg:otherArgs) (expr:otherExprs) = do
                    evaluatedExpr <- runExpr expr
                    (preparedEnv, changed) <- prepareFunctionEnv functionEnv otherArgs otherExprs
                    case evaluatedExpr of
                        Nothing -> throwError "Argument needs to evaluate to a value."
                        (Just e) -> let
                                    currentVal = mem ! (preparedEnv ! snd arg)
                            in if getType mem preparedEnv currentVal == getType mem preparedEnv e then do
                                case arg of
                                    (False, ident) -> do
                                        let modifiedMap = insert (preparedEnv ! ident) (mem ! (preparedEnv ! ident)) changed
                                        modify $ insert (preparedEnv ! ident) (head $ maybeToList evaluatedExpr)
                                        return (preparedEnv, modifiedMap)
                                    (True, ident) ->
                                        case expr of
                                            (EVar varIdent) -> do
                                                return (insert ident (env ! varIdent) preparedEnv, changed)
                                            _ -> throwError "Reference argument needs to be a variable."
                            else
                                throwError "Wrong type."

                prepareFunctionEnv functionEnv [] [] = return (functionEnv, empty)
            in
            if member ident env then do
                -- myPrint $ show ident ++ " | " ++ show env ++ " | " ++ show mem
                currentMem <- get
                let
                    -- (VFun fType block env args) = mem ! (env ! ident)
                    (VFun function) = mem ! (env ! ident)
                -- in
                case function of
                    Nothing -> throwError "Function has not been initialized."
                    (Just (ValidFun fType block env args)) ->
                        if length args == length exprs then do
                            (preparedEnv, changed) <- prepareFunctionEnv env args exprs
                            if ident == Ident "main" then do
                                returnVal <- local (const preparedEnv) (runFunBlock True fType block)
                                modify $ union changed 
                                if getType mem env returnVal == fType then
                                    return $ Just returnVal
                                else
                                    throwError "Wrong returned type."
                            else do
                                returnVal <- local (const preparedEnv) (runFunBlock False fType block)
                                modify $ union changed
                                if getType mem env returnVal == fType then
                                    return $ Just returnVal
                                else
                                    throwError "Wrong returned type."
                                return $ Just returnVal
                        else
                            throwError "Number of arguments doesn't match."
            else
                throwError $ "Function " ++ show ident ++ " doesn't exist."
        (EString string) -> do
            return $ Just $ VStr string
        (Neg expr) -> do
            evaluatedExpr <- runExpr expr
            case evaluatedExpr of
                (Just (VInt int)) -> return $ Just $ VInt (-int)
                _ -> throwError "'-' can anly be applied for int type."
        (Not expr) -> do
            evaluatedExpr <- runExpr expr
            case evaluatedExpr of
                (Just (VBool bool)) -> return $ Just $ VBool (not bool)
                _ -> throwError "'!' can anly be applied for bool type."
        (EMul expr1 mulOp expr2) -> do
            evalExpr1 <- runExpr expr1
            evalExpr2 <- runExpr expr2
            case (evalExpr1, evalExpr2) of
                (Just (VInt int1), Just (VInt int2)) -> case mulOp of
                    Times -> return $ Just $ VInt (int1 * int2)
                    Div -> 
                        if int2 == 0 then
                            throwError "Cannot divide by 0."
                        else
                            return $ Just $ VInt (int1 `div` int2)
                    Mod -> return $ Just $ VInt (int1 `mod` int2)
                _ -> throwError "this operation can anly be applied for int types."
        (EAdd expr1 addOp expr2) -> do
            evalExpr1 <- runExpr expr1
            evalExpr2 <- runExpr expr2
            case (evalExpr1, evalExpr2) of
                (Just (VInt int1), Just (VInt int2)) -> case addOp of
                    Plus -> return $ Just $ VInt (int1 + int2)
                    Minus -> return $ Just $ VInt (int1 - int2)
                (Just (VStr str1), Just (VStr str2)) -> case addOp of
                    Plus -> return $ Just $ VStr (str1 ++ str2)
                    Minus -> throwError "Cannot subtract strings."
                _ -> throwError "this operation cannot be applied for this type."
        (ERel expr1 relOp expr2) -> do
            evalExpr1 <- runExpr expr1
            evalExpr2 <- runExpr expr2
            case (evalExpr1, evalExpr2) of
                (Just (VInt int1), Just (VInt int2)) -> case relOp of
                    LTH -> return $ Just $ VBool (int1 < int2)
                    LE -> return $ Just $ VBool (int1 <= int2)
                    GTH -> return $ Just $ VBool (int1 > int2)
                    GE -> return $ Just $ VBool (int1 >= int2)
                    EQU -> return $ Just $ VBool (int1 == int2)
                    NE -> return $ Just $ VBool (int1 /= int2)
                _ -> case relOp of
                    EQU -> return $ Just $ VBool (evalExpr1 == evalExpr2)
                    NE -> return $ Just $ VBool (evalExpr1 /= evalExpr2)
                    _ -> throwError "this operation can anly be applied for int types."
        (EAnd expr1 expr2) -> do
            evalExpr1 <- runExpr expr1
            evalExpr2 <- runExpr expr2
            case (evalExpr1, evalExpr2) of
                (Just (VBool bool1), Just (VBool bool2)) ->
                    return $ Just $ VBool (bool1 && bool2) 
                _ -> throwError "this operation can anly be applied for bool types."
        (EOr expr1 expr2) -> do
            evalExpr1 <- runExpr expr1
            evalExpr2 <- runExpr expr2
            case (evalExpr1, evalExpr2) of
                (Just (VBool bool1), Just (VBool bool2)) ->
                    return $ Just $ VBool (bool1 || bool2) 
                _ -> throwError "this operation can anly be applied for bool types."

runMainExpr :: Expr -> RunnerRetType (Maybe VariableType)
runMainExpr e = do runExpr e

runItems :: Type -> [Item] -> RunnerRetType Env
runItems itemType i = let
        processItems :: Env -> [Item] -> RunnerRetType Env
        processItems currentEnv (item:next) = case item of
            (NoInit ident) -> do
                resultEnv <- processItems currentEnv next
                mem <- get 
                zero <- getZero itemType
                let
                    location = newLocation mem
                    modifiedEnv = insert ident location resultEnv
                if isNothing zero then
                    throwError "This type needs to be initialized."
                else do
                    modify $ insert location (head $ maybeToList zero)
                    return modifiedEnv
            (Init ident expr) -> do
                resultEnv <- processItems currentEnv next
                mem <- get
                processedExpr <- runExpr expr
                let
                    location = newLocation mem
                    modifiedEnv = insert ident location resultEnv
                if isNothing processedExpr then do
                    throwError "Variable cannot be initialized with this value."
                else do
                    modify $ insert location (head $ maybeToList processedExpr)
                    return modifiedEnv

        processItems currentEnv [] = return currentEnv
    in do
        env <- ask 
        processItems env i

runFunBlock :: Bool -> Type -> Block -> RunnerRetType VariableType
runFunBlock isMain retType b = do 
    (_, returnValue) <- runStmt (BStmt b) True False
    if isNothing returnValue then do
        if isMain then 
            return (VInt 0)
        else
            case retType of
                Void -> return VVoid
                _ -> throwError "This function must return a value."
    else
        return $ snd $ head $ maybeToList returnValue


runStmt :: Stmt -> Bool -> Bool -> RunnerRetType (Env, Maybe (StmtReturnType, VariableType))
runStmt s isFunction isLoop = do
    env <- ask 
    mem <- get 
    case s of
        Empty -> return (env, Nothing)
        (BStmt (Block stmts)) -> let
                runAllStmts :: [Stmt] -> RunnerRetType (Env, Maybe (StmtReturnType, VariableType))
                runAllStmts (stmt:next) = do
                    (newEnv, evaluatedStmt) <- runStmt stmt isFunction isLoop
                    case evaluatedStmt of
                        Nothing -> do local (const newEnv) (runAllStmts next)
                        (Just (StmtBreak, v)) -> 
                            if not isLoop then
                                throwError "Cannot use break outside the loop."
                            else
                                return (newEnv, Just (StmtBreak, v))
                        (Just (StmtContinue, v)) ->
                            if not isLoop then
                                throwError "Cannot use continue outside the loop."
                            else
                                return (newEnv, Just (StmtContinue, v))
                        (Just (StmtFunctionReturn, v)) -> 
                            if not isFunction then
                                throwError "Cannot use return outside the function."
                            else
                                return (newEnv, Just (StmtFunctionReturn, v))

                runAllStmts [] = do 
                    currentEnv <- ask
                    return (currentEnv, Nothing)
            in do 
                (_, r) <- runAllStmts stmts
                env <- ask 
                return (env, r)
        (FnStmt topDef) -> do
            case topDef of
                (FnDef retType ident args block) ->
                    if ident == Ident "main" then
                        throwError "Cannot define another main() function."
                    else do
                        (newEnv, _) <- runTopDef topDef
                        return (newEnv, Nothing)
                (VarDef init) -> do
                    (newEnv, _) <- runTopDef (VarDef init)
                    return (newEnv, Nothing)
        (Decl varType items) -> do
            (newEnv, _) <- runTopDef (VarDef (TopInit varType items))
            return (newEnv, Nothing)
        (Ass ident expr) -> do
            evaluatedExpr <- runExpr expr
            case evaluatedExpr of
                Nothing -> throwError "Assigned value must be a valid value."
                (Just v) ->
                    if member ident env then do
                        if getType mem env (mem ! (env ! ident)) == getType mem env v then do
                            modify $ insert (env ! ident) v
                            return (env, Nothing)
                        else
                            throwError "Types don't match."
                    else
                        throwError $ "Variable " ++ show ident ++ " doesn't exist."
        (ArrAss ident index expr) -> do
            evaluatedIndex <- runExpr index
            case evaluatedIndex of
                (Just (VInt i)) -> do
                    evaluatedExpr <- runExpr expr
                    case evaluatedExpr of
                        Nothing -> throwError "Assigned value must be a valid value."
                        (Just v) -> 
                            if member ident env then let
                                    (VArr arrType arr) = mem ! (env ! ident)
                                in
                                if arrType == getType mem env v then do
                                    case mem ! (env ! ident) of
                                        (VArr arrType arr) -> 
                                            if i >= 0 && i <= snd (Data.Array.bounds arr) then do
                                                let modifiedArr = arr Data.Array.// [(i, v)]
                                                modify $ insert (env ! ident) (VArr arrType modifiedArr)
                                                return (env, Nothing)
                                            else
                                                throwError "Index out of bounds."
                                        (VTuple arr) -> 
                                            if i >= 0 && i <= snd (Data.Array.bounds arr) then do
                                                let modifiedArr = arr Data.Array.// [(i, v)]
                                                modify $ insert (env ! ident) (VTuple modifiedArr)
                                                return (env, Nothing)
                                            else
                                                throwError "Index out of bounds."
                                else
                                    throwError "Wrong type."
                            else
                                throwError $ "Array " ++ show ident ++ " does not exist."
                _ -> throwError "Index must evaluate to int."
        (TupleAss idents expr) -> do
            evaluatedExpr <- runExpr expr
            case evaluatedExpr of
                Nothing -> throwError "Assigned value must be a valid value."
                (Just (VTuple arr)) -> let
                        createVariables :: [VariableType] -> [Ident] -> RunnerRetType Env
                        createVariables (varValue:nextValues) (varIdent:nextIdents) = do
                            newEnv <- createVariables nextValues nextIdents
                            mem <- get
                            if member varIdent newEnv then do
                                let
                                    location = newEnv ! varIdent
                                    modifiedEnv = insert varIdent location newEnv
                                if getType mem newEnv (mem ! location) == getType mem newEnv varValue then do
                                    modify $ insert location varValue
                                    return modifiedEnv
                                else
                                    throwError "Wrong type."
                            else do
                                let
                                    location = newLocation mem
                                    modifiedEnv = insert varIdent location newEnv
                                modify $ insert location varValue
                                return modifiedEnv
                        
                        createVariables [] [] = do return env
                    in do
                        newEnv <- createVariables (Data.Array.elems arr) idents
                        return (newEnv, Nothing)
        (Incr ident) ->
            if member ident env then
                case mem ! (env ! ident) of
                    (VInt i) -> do
                        modify $ insert (env ! ident) (VInt (i + 1))
                        return (env, Nothing)
                    _ -> throwError "Only int type can be incremented."
            else
                throwError $ "Variable " ++ show ident ++ " doesn't exist."
        (Decr ident) ->
            if member ident env then
                case mem ! (env ! ident) of
                    (VInt i) -> do
                        modify $ insert (env ! ident) (VInt (i - 1))
                        return (env, Nothing)
                    _ -> throwError "Only int type can be incremented."
            else
                throwError $ "Variable " ++ show ident ++ " doesn't exist."
        (Print expr) -> do
            case expr of
                (EString s) -> do
                    liftIO $ putStr $ s ++ "\n"
                    return (env, Nothing)
                _ -> do
                    evaluatedExpr <- runExpr expr
                    case evaluatedExpr of
                        Nothing -> do
                            liftIO $ putStr "None\n"
                            return (env, Nothing)
                        (Just (VStr s)) -> do
                            liftIO $ putStr $ show s ++ "\n"
                            return (env, Nothing)
                        (Just e) -> do
                            liftIO $ putStr $ show e ++ "\n"
                            return (env, Nothing)
        (Ret expr) -> do
            evaluatedExpr <- runExpr expr
            if isFunction then
                case evaluatedExpr of
                    Nothing -> return (env, Just (StmtFunctionReturn, VVoid))
                    (Just e) -> return (env, Just (StmtFunctionReturn, e))
            else
                throwError "Cannot use return outside a function."
        VRet -> do
            if isFunction then
                return (env, Just (StmtFunctionReturn, VVoid))
            else
                throwError "Cannot use return outside a function."
        (Cond expr stmt) -> do
            evaluatedExpr <- runExpr expr
            case evaluatedExpr of
                (Just (VBool True)) -> do runStmt stmt isFunction isLoop
                (Just (VBool False)) -> return (env, Nothing)
                _ -> throwError "Expression inside if needs to evaluate to bool."
        (CondElse expr stmt1 stmt2) -> do
            evaluatedExpr <- runExpr expr
            case evaluatedExpr of
                (Just (VBool True)) -> do runStmt stmt1 isFunction isLoop
                (Just (VBool False)) -> do runStmt stmt2 isFunction isLoop
                _ -> throwError "Expression inside if needs to evaluate to bool."
        (While expr stmt) -> do
            evaluatedExpr <- runExpr expr
            case evaluatedExpr of
                (Just (VBool True)) -> do 
                    (newEnv, retVal) <- runStmt stmt isFunction True
                    case retVal of
                        Nothing -> local (const newEnv) (runStmt (While expr stmt) isFunction isLoop)
                        (Just (StmtBreak, _)) -> return (env, Nothing)
                        (Just (StmtContinue, _)) -> local (const newEnv) (runStmt (While expr stmt) isFunction isLoop)
                        (Just (StmtFunctionReturn, r)) -> return (env, Just (StmtFunctionReturn, r))
                (Just (VBool False)) -> do return (env, Nothing)
                _ -> throwError "Expression inside while needs to evaluate to bool."
        (Resize ident integer) -> do
            (Just (VInt size)) <- runExpr (ESize ident)
            (Just (VArr arrType arr)) <- runExpr (EVar ident)
            (Just zero) <- getZero arrType
            if fromInteger integer > size then let
                    additionalElems = replicate (fromInteger integer - size) zero
                    newArr = Data.Array.elems arr ++ additionalElems
                in do
                    modify $ insert (env ! ident) (VArr arrType (Data.Array.listArray (0, fromInteger integer - 1) newArr))
                    return (env, Nothing)
            else let
                    newArr = Prelude.take (fromInteger integer) (Data.Array.elems arr)
                in do
                    modify $ insert (env ! ident) (VArr arrType (Data.Array.listArray (0, fromInteger integer - 1) newArr))
                    return (env, Nothing)
        (ResizeId ident expr) -> do
            evaluatedExpr <- runExpr expr
            case evaluatedExpr of
                (Just (VInt i)) -> do runStmt (Resize ident (toInteger i)) isFunction isLoop
                _ -> throwError "Expression needs to evaluate to int."
        Break -> 
            if isLoop then
                return (env, Just (StmtBreak, VVoid))
            else
                throwError "break can only be used inside a loop."
        Continue ->
            if isLoop then
                return (env, Just (StmtContinue, VVoid))
            else
                throwError "continue can only be used inside a loop."
        (SExp expr) -> do
            evaluatedExpr <- runExpr expr
            return (env, Nothing)
