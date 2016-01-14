module Backend.X86.ASMTransformations.SimplifyStackOperations where

import Backend.X86.DataTypes

simplifyStackOperations :: [Statement] -> [Statement]
simplifyStackOperations = postprocess . simplifyStackOperationsInner . preprocess

simplifyStackOperationsInner :: [Statement] -> [Statement]
simplifyStackOperationsInner l =
  let simplified = simplifyStackOperations' l in
    if length simplified < length l then simplifyStackOperationsInner simplified else simplified

simplifyStackOperations' :: [Statement] -> [Statement]
simplifyStackOperations' l = case l of
  (SInstr (Instr1A PushL l1)):(SInstr (Instr1A PopL l2)):tl | isImmediate l1 || isImmediate l2 ->
    if l1 == l2 then
      (simplifyStackOperations' tl)
    else
      (SInstr (Instr2A MovL l1 l2)) : (simplifyStackOperations' tl)
  (SInstr (Instr2A AddL (LImm n1) l1)):(SInstr (Instr2A AddL (LImm n2) l2)):tl | l1 == l2 ->
    if n1 + n2 == 0 then
      (simplifyStackOperations' tl)
    else
      (SInstr (Instr2A AddL (LImm (n1 + n2)) l1)) : (simplifyStackOperations' tl)
  (SInstr (Instr2A MovL l1 l2)):(SInstr (Instr2A MovL l2' l3)):tl | l2 == l2' && isImmediate l1 && isImmediate l2 ->
    if l1 == l2 then
      (simplifyStackOperations' tl)
    else
      (SInstr (Instr2A MovL l1 l3)) : (simplifyStackOperations' tl)
  h:t -> h:(simplifyStackOperations' t)
  _   -> l

preprocess :: [Statement] -> [Statement]
preprocess = invertSubls . removeComments

removeComments :: [Statement] -> [Statement]
removeComments = filter (\x -> case x of { SComment b _ -> not b; _ -> True })

invertSubls :: [Statement] -> [Statement]
invertSubls = map (\x -> case x of { SInstr (Instr2A SubL (LImm n) l) -> SInstr (Instr2A AddL (LImm (-n)) l); _ -> x })

postprocess :: [Statement] -> [Statement]
postprocess = normalizeSubls

normalizeSubls :: [Statement] -> [Statement]
normalizeSubls = map (\x -> case x of {
    SInstr (Instr2A SubL (LImm n) l) | n < 0 -> SInstr (Instr2A AddL (LImm (-n)) l);
    SInstr (Instr2A AddL (LImm n) l) | n < 0 -> SInstr (Instr2A SubL (LImm (-n)) l);
    _ -> x
  })