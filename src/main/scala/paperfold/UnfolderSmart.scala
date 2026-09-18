package paperfold

type Score = Int
type Cost = Int
type PaperShape = Array[Array[String]]

object UnfolderSmart:
    def instructionsFor(target: String): List[Instruction] =
        require(target.nonEmpty, "cannot unfold an empty paper")
        val n = target.length
        val start = enqueueShapes(n, 1, Nil)
        //println(s"enqueueShapes" + start.length);
        //displayShapes(start)
        val (result, cost) = runMultiShape(start, target, List[(List[Instruction], Score)](), 0)
        println(s"cost: " + cost + ", result" + result);
        result(0)(0)

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

    def runMultiShape(shapes: List[PaperShape], target: String, result: List[(List[Instruction], Score)], cost: Cost): (List[(List[Instruction], Score)], Cost) =
        shapes match
            case Nil => (result, cost)
            case head :: rest => 
                val (resultRunShape, costRun) = run(head, target)
                runMultiShape(rest, target, resultRunShape, cost + costRun)

    def run(paper: PaperShape, target: String): (List[(List[Instruction], Score)], Cost)  = 
        // generates random (first pool)
        // TODO : first random pool
        val firstPool = List(
            List(Instruction('R', 2), Instruction('T', 1), Instruction('R', 1), Instruction('B', 1)),
            List(Instruction('L', 2), Instruction('R', 1), Instruction('B', 2), Instruction('L', 1)),
        )
        // evaluate first pool
        val (evaluatedFirstPool, costEval) = evaluate(paper, firstPool, target, 0, List[(List[Instruction], Score)]())
        // order list
        val orderedPool = sortPool(evaluatedFirstPool, List[(List[Instruction], Score)]())
        // run 10 generations
        val (finalPool, cost) = nextGeneration(10, paper, target, orderedPool, 0)
        (finalPool, costEval + cost)

    def nextGeneration(nbGen: Int, paper: Array[Array[String]], target: String, pool: List[(List[Instruction], Score)], cost: Int): (List[(List[Instruction], Score)], Cost) =
        // stop condition
        if (nbGen > 0) then 
            // remove zero-score
            val pool_no_zero = removeZeroScored(pool)
            // order list (NOT NEEDED ANYMORE because it is ordered by construct)
            // val orderedPool = sortPool(pool_no_zero, List[(List[Instruction], Score)]())
            // takes 5 best
            val bests = if pool_no_zero.length > 5 then pool_no_zero.take(5) else pool_no_zero 
            // generate siblings
            val siblings = generateSiblings(5, pool, List[List[Instruction]]())
            // evaluate siblings
            val (evaluatedSiblings, costEvalSiblings) = evaluate(paper, siblings, target, 0, List[(List[Instruction], Score)]())
            val newPool = insertManyInPool(evaluatedSiblings, bests)
            nextGeneration(nbGen - 1, paper, target, newPool, cost + costEvalSiblings)
        else
            (pool, cost)

    // TODO: add new individuals by cross-over, mutation, random
    def generateSiblings(nb: Int, pool: List[(List[Instruction], Score)], bag: List[List[Instruction]]): List[List[Instruction]] =
        List[List[Instruction]]()

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
        val folded = Folder.applyAll(paper, instructions)
        assert(folded.length == 1)
        assert(folded.head.length == 1)
        val computed = folded(0)(0)
        (similarityScore(computed.toList, target.toList, 0), instructions.length)

    def similarityScore(computed: List[Char], expected: List[Char], score: Score): Score =
        computed match
            case c :: cn :: crest => 
                if expected.contains(cn.toString + c.toString) then 
                    similarityScore(cn :: crest, expected, score + 1)
                else
                    if expected.contains(c.toString + cn.toString) then 
                        similarityScore(cn :: crest, expected, score + 2)
                    else
                        similarityScore(cn :: crest, expected, score)
            case _ => score
    

    private def reverse[A](list: List[A]): List[A] =
        def go(remaining: List[A], acc: List[A]): List[A] =
        remaining match
            case Nil          => acc
            case head :: tail => go(tail, head :: acc)
        go(list, Nil)