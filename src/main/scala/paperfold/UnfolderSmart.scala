package paperfold

type Score = Int
type Cost = Int
type PaperShape = Array[Array[String]]

import scala.util.Random

object UnfolderSmart:
    def instructionsFor(target: String): (List[Instruction], Score, (Int, Int)) =
        require(target.nonEmpty, "cannot unfold an empty paper")
        val n = target.length
        val start = enqueueShapes(n, 1, Nil)
        val sortedShapes = sortShapes(start, List[Array[Array[String]]]())
        val (result, cost) = runMultiShape(sortedShapes, target, List[(List[Instruction], Score, (Int, Int))](), 0)
        println(s"total cost: " + cost + ", result" + result)
        getBest(0, result, (Nil, 0, (1, 1)))

    def getBest(bestScore: Int, result: List[(List[Instruction], Score, (Int, Int))], best: (List[Instruction], Score, (Int, Int)) ) : (List[Instruction], Score, (Int, Int)) =
        result match
            case elt :: rest => 
                if elt(1) > bestScore then
                    getBest(elt(1), rest, elt)
                else
                    getBest(bestScore, rest, best)
            case Nil => best

    private def enqueueShapes(
        n: Int,
        rows: Int,
        acc: List[Array[Array[String]]]
    ): List[Array[Array[String]]] =
        if rows > n then 
            reverse(acc)
        else if n % rows == 0 then
            val cols = n / rows
            enqueueShapes(n, rows + 1, Paper.create(rows, cols) :: acc)
        else 
            enqueueShapes(n, rows + 1, acc)

    def sortShapes(shapes: List[Array[Array[String]]], sorted: List[Array[Array[String]]]): List[Array[Array[String]]] = 
        shapes match
            case Nil => sorted
            case shape :: rest => 
                // remove 1x shapes
                if Paper.height(shape) == 1 || Paper.width(shape) == 1 then
                    sortShapes(rest, sorted)
                else    
                    sortShapes(rest, insertInShapes(shape, sorted))

    def insertInShapes(elt: Array[Array[String]], shapes: List[Array[Array[String]]]): List[Array[Array[String]]] = 
        shapes match
            case shape :: rest =>
                val eltDims = Paper.height(elt) + Paper.width(elt)
                val shapeDims = Paper.height(shape) + Paper.width(shape)
                if eltDims < shapeDims then 
                    elt :: shape :: rest
                else
                    shape :: insertInShapes(elt, rest)
            case Nil => elt :: Nil

    def runMultiShape(shapes: List[PaperShape], target: String, result: List[(List[Instruction], Score, (Int, Int))], cost: Cost): (List[(List[Instruction], Score, (Int, Int))], Cost) =
        shapes match
            case Nil => (result, cost)
            case head :: rest => 
                val (resultRunShape, costRun) = run(head, target)
                if resultRunShape.length > 0 then
                    println("costRun: " + costRun + " ,result best: " + resultRunShape(0) + " for shape " + + Paper.height(head) + "x" + Paper.width(head))
                else
                    println("no result !")
                if (resultRunShape(0)(1) == 10000) then
                    ((resultRunShape(0)(0), resultRunShape(0)(1), (Paper.height(head), Paper.width(head))) :: result, cost + costRun)
                else
                    runMultiShape(rest, target, (resultRunShape(0)(0), resultRunShape(0)(1), (Paper.height(head), Paper.width(head))) :: result, cost + costRun)

    def run(paper: PaperShape, target: String): (List[(List[Instruction], Score)], Cost)  = 
        // generates random (first pool)
        val (firstPool, costRandChildren) = generateSiblingsByRandom(5, paper, Nil, 0)
        // evaluate first pool
        val (evaluatedFirstPool, costEval) = evaluate(paper, firstPool, target, 0, List[(List[Instruction], Score)]())
        // order list
        val orderedPool = sortPool(evaluatedFirstPool, List[(List[Instruction], Score)]())
        // run 10 generations
        val (finalPool, cost) = nextGeneration(800, paper, target, orderedPool, 0)
        println("shape: " + Paper.height(paper) + "x" + Paper.width(paper) + " ,best: " + finalPool(0))
        (finalPool, costRandChildren + costEval + cost)

    def nextGeneration(nbGen: Int, paper: Array[Array[String]], target: String, pool: List[(List[Instruction], Score)], cost: Int): (List[(List[Instruction], Score)], Cost) =
        // stop condition
        //println("generation: " + nbGen + ", pool size: " + pool.length + ", best: " + pool(0)(1))
        if (pool(0)(1) == 10000) then 
            (pool, cost)
        else
            if (nbGen > 0) then 
                // remove zero-score
                val pool_no_zero = removeZeroScored(pool)
                // order list (NOT NEEDED ANYMORE because it is ordered by construct)
                // val orderedPool = sortPool(pool_no_zero, List[(List[Instruction], Score)]())
                // takes 10 best
                //val bests = if pool_no_zero.length > 10 then pool_no_zero.take(10) else pool_no_zero 
                val bests = if nbGen % 5 == 0 then 
                    removeDuplicatesAdjacent(pool_no_zero).take(15)
                else
                    pool_no_zero.take(15)
                //println("bests: " + bests.map(elt => elt(1)).toString)                
                // generate siblings
                val people = pool.map( elt => elt(0))
                val (randChildren, costRandChildren) = generateSiblingsByRandom(5, paper, Nil, 0)
                val siblings = generateSiblings(20, paper, people ++ randChildren, List[List[Instruction]]())
                // evaluate siblings
                val (evaluatedSiblings, costEvalSiblings) = evaluate(paper, siblings ++ randChildren, target, 0, List[(List[Instruction], Score)]())
                //val (evaluatedSiblings, costEvalSiblings) = evaluate(paper, randChildren, target, 0, List[(List[Instruction], Score)]())
                val newPool = insertManyInPool(evaluatedSiblings, bests)
                nextGeneration(nbGen - 1, paper, target, newPool, cost + costEvalSiblings + costRandChildren)
            else
                (pool, cost)

    // TODO: add new individuals by cross-over, mutation, random
    def generateSiblings(nb: Int, paper: Array[Array[String]], pool: List[List[Instruction]], bag: List[List[Instruction]]): List[List[Instruction]] =
        val children = generateSiblingsByCrossover(nb / 3, Random.shuffle(pool))
        val childrenMulti = generateSiblingsByCrossoverMulti(nb / 3, Random.shuffle(pool))
        val mutatedChildren = generateSiblingsByMutation(nb / 3, pool)
        childrenMulti ++ children ++ mutatedChildren

    def generateSiblingsByRandom(nb:Int, paper: Array[Array[String]], result: List[List[Instruction]], cost: Cost) : (List[List[Instruction]], Cost) =
        nb match
            case 0 => (result, cost)
            case nb => 
                // add a random child
                val (oneRandomChild, costRandom) = generateSiblingByRandom(paper)
                generateSiblingsByRandom(nb - 1, paper, oneRandomChild :: result, cost + costRandom)
    
    private def range(from: Int, untilExclusive: Int, part: Char): List[Instruction] =
        if from >= untilExclusive then Nil
        else Instruction(part, from) :: range(from + 1, untilExclusive, part)

    private def candidates(paper: Array[Array[String]]): List[Instruction] =
        val cols = Paper.width(paper)
        val rows = Paper.height(paper)
        range(1, cols, 'L') ++ range(1, cols, 'R') ++ range(1, rows, 'T') ++ range(1, rows, 'B')

    // pick a random instructions from candidates
    def generateInstruction(candidates: List[Instruction]): Instruction =
        val n = Random.nextInt(1000) % candidates.length
        candidates(n)

    def generateInstructions(paper: Array[Array[String]], result: List[Instruction], cost: Cost): (List[Instruction], Cost) =
        if (Paper.height(paper) > 1) || Paper.width(paper) > 1 then
            val oneInstruction = generateInstruction(candidates(paper))
            val folded = Folder.fold(paper, oneInstruction)
            generateInstructions(folded, oneInstruction :: result, cost + 1)
        else 
            (reverse(result), cost)
 
    def generateSiblingByRandom(paper: Array[Array[String]]): (List[Instruction], Cost) =
        generateInstructions(paper, Nil, 0)

    def generateSiblingsByMutation(nb : Int, pool: List[List[Instruction]]) : List[List[Instruction]] =
        pool match
            case Nil => Nil
            case head :: rest => 
                if nb > 0 then
                    generateSiblingByMutation(head) :: generateSiblingsByMutation(nb - 1, rest)
                else
                    Nil
    
    def generateSiblingByMutation(instructions: List[Instruction]): List[Instruction] = 
        instructions match
            case Nil => Nil
            case headInst :: restInst =>
                if Random.nextInt(100) % 2 > 0 then
                    val newPart = headInst.part match
                        case 'L' => 'R'
                        case 'R' => 'L'
                        case 'B' => 'T'
                        case 'T' => 'B'
                    val newIndex = 
                        if Random.nextInt(100) % 2 > 0 then
                            headInst.index + 1
                        else 
                            if headInst.index > 2 then 
                                headInst.index - 1
                            else
                                headInst.index + 1
                    Instruction(newPart, newIndex) :: generateSiblingByMutation(restInst)
                else
                    headInst :: generateSiblingByMutation(restInst)

    def generateSiblingsByCrossover(nb:Int, pool: List[List[Instruction]]) : List[List[Instruction]] = 
        pool match
            case Nil => Nil
            case p :: Nil => Nil
            case p :: s :: rest => 
                if nb > 0 then
                    generateSiblingByCrossover(p, s) ++ generateSiblingsByCrossover(nb - 1, rest)
                else
                    Nil

    def generateSiblingByCrossover(parent1: List[Instruction], parent2: List[Instruction]): List[List[Instruction]] =
        val index = (Random.nextInt(100) % parent1.length) + 1
        val one = parent1.take(index) ++ parent2.drop(index)
        val two = parent2.take(index) ++ parent1.drop(index)
        one :: two :: Nil


    def generateSiblingsByCrossoverMulti(nb:Int, pool: List[List[Instruction]]) : List[List[Instruction]] = 
        pool match
            case Nil => Nil
            case p :: Nil => Nil
            case p :: s :: rest => 
                if nb > 0 then
                    generateSiblingByCrossoverMulti(p, s) :: generateSiblingsByCrossoverMulti(nb - 1, rest)
                else
                    Nil

    def generateSiblingByCrossoverMulti(parent1: List[Instruction], parent2: List[Instruction]): List[Instruction] =
        val genA = parent1
        val genB = parent2
        (genA, genB) match 
            case (a :: restA, b :: restB) => 
                if (Random.nextInt(10) % 2 > 0) then 
                    a :: generateSiblingByCrossoverMulti(restA, restB) 
                else 
                    b :: generateSiblingByCrossoverMulti(restA, restB)
            case (a :: restA, Nil) => a :: generateSiblingByCrossoverMulti(restA, Nil)
            case (Nil, b :: restB) => b :: generateSiblingByCrossoverMulti(Nil, restB)
            case (Nil,Nil) => Nil

    def evaluate(paper: Array[Array[String]], instsList: List[List[Instruction]], target: String, cost: Cost, evaluated: List[(List[Instruction], Score)]): (List[(List[Instruction], Score)], Cost) = 
        val (evaluatedPool, finalcost) = instsList match
            case Nil => (evaluated, cost)
            case instructions :: rest => 
                val (score, costFitness) = fitness(paper, instructions, target)
                evaluate(paper, rest, target, cost + costFitness, (instructions, score) :: evaluated)
        (evaluatedPool, cost + finalcost)

    def removeZeroScored(pool: List[(List[Instruction], Score)]): List[(List[Instruction], Score)] =
        pool match
            case Nil => Nil
            case (insts, 0) :: rest => removeZeroScored(rest)
            case (insts, score) :: rest =>  (insts, score) :: removeZeroScored(rest)

    def removeDuplicatesAdjacent(pool: List[(List[Instruction], Score)]): List[(List[Instruction], Score)] =
        pool match
            case Nil => Nil
            case (inst1, score1) :: (inst2, score2) :: rest =>
                if (inst1 == inst2) then 
                    removeDuplicatesAdjacent((inst2, score2) :: rest) 
                else
                    (inst1, score1) :: removeDuplicatesAdjacent((inst2, score2) :: rest) 
            case (insts, score) :: Nil =>  (insts, score) :: Nil

    def sortPool(pool: List[(List[Instruction], Score)], sorted: List[(List[Instruction], Score)]): List[(List[Instruction], Score)] = 
        pool match
            case Nil => sorted
            case (instructions, score) :: rest => 
                sortPool(rest, insertInPool((instructions, score), sorted))

 

    def insertInPool(elt: (List[Instruction], Score), pool: List[(List[Instruction], Score)]): List[(List[Instruction], Score)] = 
        pool match
            case (insts, score) :: rest => 
                if elt(1) < score then 
                    (insts, score) :: insertInPool(elt, rest)
                else
                    elt :: (insts, score) :: rest
            case Nil => elt :: Nil
    
    def insertManyInPool(newones: List[(List[Instruction], Score)], pool: List[(List[Instruction], Score)]): List[(List[Instruction], Score)] = 
        newones match
            case Nil => pool
            case head :: rest => insertManyInPool(rest, insertInPool(head, pool))

    // returns similarity score and cost (in number of folding step)
    def fitness(paper: Array[Array[String]], instructions: List[Instruction], target: String): (Int, Int) =
        try {        
            val folded = Folder.applyAll(paper, instructions)
            if (folded.length == 1) && (folded.head.length == 1) then 
                val computed = folded(0)(0)
                (similarityScore(computed.toList, target.toList, 0), instructions.length)
            else
                //println("Wrong number of instructions: " + instructions.length + " -> " + Paper.height(folded) + "x" + Paper.width(folded))
                (0, instructions.length)
        } catch {
            case e => (0, instructions.length)
        } 

    def similarityScore(computed: List[Char], expected: List[Char], score: Score): Score =
        if computed.mkString == expected.mkString then
            10000
        else
            computed match
                case c :: cn :: cn2 :: crest =>
                    if expected.mkString.contains(c.toString + cn.toString + cn2.toString) then 
                        similarityScore(cn :: cn2 :: crest, expected, score + 4)
                    else
                        if expected.mkString.contains(cn.toString + c.toString) then 
                            similarityScore(cn :: cn2 :: crest, expected, score + 1)
                        else
                            if expected.mkString.contains(c.toString + cn.toString) then 
                                similarityScore(cn :: cn2 :: crest, expected, score + 2)
                            else
                                similarityScore(cn :: cn2 :: crest, expected, score)
                case c :: cn :: Nil => 
                    if expected.mkString.contains(cn.toString + c.toString) then 
                        score + 1
                    else
                        if expected.mkString.contains(c.toString + cn.toString) then 
                            score + 2
                        else
                            score
                case _ => score
    

    private def reverse[A](list: List[A]): List[A] =
        def go(remaining: List[A], acc: List[A]): List[A] =
        remaining match
            case Nil          => acc
            case head :: tail => go(tail, head :: acc)
        go(list, Nil)