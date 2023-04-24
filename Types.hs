module Types where

import AbsGrammar
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe
import Data.Map
import Data.Array
import qualified Data.String

data ValidFunction = ValidFun Type Block Env [(Bool, Ident)]
    deriving (Eq, Show)

data VariableType
    = VInt Int
    | VStr String
    | VBool Bool
    | VVoid
    | VArr Type (Array Int VariableType)
    | VTuple (Array Int VariableType) 
    | VFun (Maybe ValidFunction)
    deriving (Eq)

instance Show VariableType where
    show (VInt i) = show i
    show (VStr s) = show s
    show (VBool b) = show b
    show VVoid = show "Void"
    show (VArr t a) = show (Data.Array.elems a)
    show (VTuple a) = let
            str = show (Data.Array.elems a)
        in "(" ++ Prelude.take (length str - 2) (Prelude.drop 1 str) ++ ")"
    show (VFun Nothing) = show "Not initialised function"
    show (VFun (Just (ValidFun fType fBlock fEnv fArgs))) = let
            showArgs (arg:nextArgs) =
                if fst arg then
                    show (snd arg) ++ ", " ++ showArgs nextArgs
                else
                    show "&" ++ show (snd arg) ++ ", " ++ showArgs nextArgs
            showArgs [] = show ""
            argsStr = showArgs fArgs
        in show fType ++ "(" ++ show (Prelude.take (length argsStr - 1) argsStr) ++ ")"

type Env = Map Ident Int
type Mem = Map Int VariableType
type FinalType = ExceptT String IO
type ReaderType = ReaderT Env FinalType
type StateType = StateT Mem ReaderType
type RunnerRetType = StateType 

data StmtReturnType = StmtBreak | StmtContinue | StmtFunctionReturn

getZero :: Type -> RunnerRetType (Maybe VariableType) 
getZero t = case t of
    Int -> return $ Just $ VInt 0
    Str -> return $ Just $ VStr ""
    Bool -> return $ Just $ VBool False
    Void -> return $ Just VVoid 
    (Arr arrType) -> return $ Just $ VArr arrType (Data.Array.listArray (0, -1) [])
    (Tuple types) -> let
            checkValidity :: [Type] -> RunnerRetType [VariableType]
            checkValidity (value:next) = do
                arr <- checkValidity next
                varVal <- getZero value
                if isNothing varVal then
                    throwError "Tuple containing a function type must be initialized."
                else
                    return $ head (maybeToList varVal):arr
            
            checkValidity [] = do return []
        in do
            arr <- checkValidity types
            return $ Just $ VTuple (Data.Array.listArray (0, length types - 1) arr)
    (Fun funType args) -> return $ Just $ VFun Nothing


getType :: Mem -> Env -> VariableType -> Type
getType mem env v = case v of
    (VInt _) -> Int 
    (VStr _) -> Str
    (VBool _) -> Bool 
    VVoid -> Void 
    (VArr arrType _) -> Arr arrType
    (VTuple arr) -> Tuple (Prelude.map (getType mem env) (Data.Array.elems arr))
    (VFun f) -> case f of
        Nothing -> Fun Void []
        (Just (ValidFun fType _ fEnv fArgs)) -> let
                getArgs :: [(Bool, Ident)] -> [Arg]
                getArgs (arg:next) = let
                        ident = snd arg
                    in if fst arg then
                        RefArg (getType mem fEnv (mem Data.Map.! (fEnv Data.Map.! ident))) ident:getArgs next
                    else
                        Arg (getType mem fEnv (mem Data.Map.! (fEnv Data.Map.! ident))) ident:getArgs next

                getArgs [] = []
            in Fun fType (getArgs fArgs)