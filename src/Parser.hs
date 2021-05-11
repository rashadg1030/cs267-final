{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Text



-- data RefineBoolExpr
--   = RefineBoolLit Bool
--   | RefineAnd RefineBoolExpr RefineBoolExpr
--   | RefineOr RefineBoolExpr RefineBoolExpr
--   | RefineNot RefineBoolExpr
--   | RefineGT RefineIntExpr RefineIntExpr
--   | RefineLT RefineIntExpr RefineIntExpr
--   | RefineGTE RefineIntExpr RefineIntExpr
--   | RefineLTE RefineIntExpr RefineIntExpr
--   | RefineEQ RefineIntExpr RefineIntExpr
--   | RefineNEQ RefineIntExpr RefineIntExpr

-- data RefineIntExpr
--   = RefineIntLit Int
--   | RefineAdd Int Int
--   | RefineSub Int Int
--   | RefineMul Int Int
