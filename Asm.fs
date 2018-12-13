open System;
open System.Collections.Generic;
open CSC7B;;

let base_execute = execute;;
execute <- fun instruction ->
  match instruction with 
    | ALU("idiv", Reg(a), Reg(b)) when b<>"dx" -> (* Idiv *)
      let f = aluop("idiv")
      REGS.["dx"] <- REGS.[b] % REGS.[a]
      REGS.[b] <- f(REGS.[a], REGS.[b])
    | ALU("idiv", Imm(a), Reg(b)) when b<>"dx" -> (* Idiv with Immediates *)
      let f = aluop("idiv")
      REGS.["dx"] <- REGS.[b] % a
      REGS.[b] <- f(a, REGS.[b])
    | ALU(op, Imm(a), Reg(b)) -> (* Alu    with Immediates *)
      let f = aluop(op)
      REGS.[b] <- f(a, REGS.[b])
    | MOV(Imm(a), Reg(b)) -> (*Mov Immediate to Register*)
      REGS.[b] <- a
    | MOV(Reg(a), Reg(b)) -> (*Move Reg to Reg*)
      REGS.[b] <- REGS.[a]
    | MOV(Reg(a), Mem(Reg("bx"))) -> (*Move Reg to Mem*)
      RAM.[REGS.["bx"]] <- REGS.[a]
    | MOV(Mem(Reg("bx")), Reg(a)) -> (*Move Mem to Reg*)
      REGS.[a] <- RAM.[REGS.["bx"]]
    | JNZ(a) ->
      if a>0 then (if REGS.["cx"]<>0 then pc <- a - 1) else raise (Exception("Invalid Instruction Number"))
    | JZ(a) ->
      if a>0 then (if REGS.["cx"]=0 then pc <- a - 1) else raise (Exception("Invalid Instruction Number"))
    | i -> base_execute i;;
    
let base_transoperand = transoperand;;
transoperand <- fun x ->
  match x with
  | "[bx]" -> Mem(Reg("bx"))
  |s when s.[0]='[' && s.[s.Length-1]=']' && s.Substring(1,2)<>"bx" -> raise (Exception("Illegal Memory Operand"))
  |x -> base_transoperand x;;
  
let base_translate = translate;;
translate <- fun ary -> (* Translates Instructions -> Calls Transoperand *)
  match ary with
  | [| "mov"; x; y |] ->
    MOV(transoperand x, transoperand y)
  | [| "jnz"; x |] ->
    try JNZ(int(x))
    with
    | exce -> raise (Exception("Invalid Instruction Number"))
  | [| "jz"; x |] ->
    try JZ(int(x))
    with
    | exce -> raise (Exception("Invalid Instruction Number"))
  | z -> base_translate ary;;
  
let rec peep (li: operation list) = 
  match li with
  | [] -> []
  | (MOV(a,b)::MOV(c,d)::other) when a=d && b=c ->
    (MOV(a,b)::(peep other))
  | (PUSH(a)::POP(b)::other) when a=b ->
    (peep other)
  | (PUSH(a)::POP(b)::other) ->
    (MOV(a,b)::(peep other))
  | (a::other) ->
    (a::(peep other));;

let challengePeep(li: operation list) = 
  let mutable pushes = Stack<operation>()
  let mutable pushCount = 0
  let mutable popCount = 0
  let mutable pops = Queue<operation>()
  let mutable lastOp = ""
  let mutable s = -1
  let mutable e = -1
  for i in 0 .. li.Length-1 do
    match li.[i] with
    | PUSH(Imm(a)) when pushCount=0 ->
      pushCount<-pushCount+1
      lastOp<-"PUSH"
      s<-i
      pushes.Push(li.[i])
    | PUSH(Imm(a)) when pushCount<>0 && lastOp="PUSH" ->
      pushCount<-pushCount+1
      pushes.Push(li.[i])
    | POP(Reg(a)) when popCount=0 && lastOp="PUSH" ->
      lastOp<-"POP"
      e<-i
      popCount<-popCount+1
      pops.Enqueue(POP(Reg(a)))
    | POP(Reg(a)) when popCount<>0 && popCount<pushCount && lastOp="POP" ->
      popCount<-popCount+1
      e<-i
      pops.Enqueue(POP(Reg(a)))
    | a when pushCount<>popCount ->
      pushCount<-0
      popCount<-0
      s<- -1
      e<- -1
      pushes.Clear()
      pops.Clear()
    | a -> pushCount<-pushCount (*Does Nothing*)
  //Match up Stack and Queue and create new instructions Pop Dequeue
  let mutable newOps = []
  for i in 0 .. pushCount-1 do
    let push = pushes.Pop()
    let pop = pops.Dequeue()
    match (push, pop) with
    | (PUSH(Imm(a)), POP(Reg(b))) -> 
      newOps<- List.append newOps [MOV(Imm(a),Reg(b))]  
    | a -> pushCount<-pushCount (*Does Nothing*)
  //Remove Instructions s to e and replace with newInstr  
  let mutable optimizedLi = []
  for i in 0 .. li.Length - 1 do
    if i < s || i > e then optimizedLi<-List.append optimizedLi [li.[i]]
    if i = s then optimizedLi<-List.append optimizedLi newOps
  optimizedLi;;
  
  
  
let optimizedRun() = 
  let firstline = Console.ReadLine();
  let mutable instructions = reverse [] (readprog [] firstline);
  instructions <- challengePeep instructions;
  instructions <- peep instructions;
  executeProg(instructions);;
  
  
trace_advice(optimizedRun);;