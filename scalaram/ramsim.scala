import scala.io.Source
// import scala.collection.immutable.Map

object Ram {
    type PC = Int
    type Registers = Map[Int, Int]
    type Labels = Map[String, PC]
    type Instruction = (PC, Registers, Labels) => (PC, Registers)
    type Instructions = Vector[Instruction]
    type ParserConfig = (Int, Instructions, Labels, Stream[String])


    def dump(regs: Registers): Unit = {
        regs.foldLeft(List[(Int, Int)]())((list, entry) => {
            entry :: list
        }).sortBy(_._1).foreach((entry) => {
            println("c(" + entry._1 + ") = " + entry._2)
        })
    }

    def parse: Stream[String] => Either[String, (Instructions, Labels)] = lines => {

        def tokenize(line: String): List[String] = {
            (line.indexOf('#') match {
                case -1 => line
                case i => line.substring(0, i)
            }).split("\\s+").toList.filter(_.length > 0)
        }

        def parse(config: ParserConfig)(tokens: List[String]): Either[String, ParserConfig] = {
            val (icount, instructions, labels, lines) = config
        // def parse: ParserConfig => List[String] => Either[String, ParserConfig] = (icount, instructions, labels, lines) => tokens => {
            tokens match {
                case Nil => {
                    lines match {
                        case Stream.Empty => Right((icount, instructions, labels, lines))
                        case line #:: lines => {
                            val tokens: List[String] = tokenize(line)
                            parse(icount, instructions, labels, lines)(tokens)
                        }
                    }
                }
                case label :: tokens if label.endsWith(":") => {
                    parse(icount, instructions, labels + (label.substring(0, label.length - 1) -> icount), lines)(tokens)
                }
                case "END" :: Nil => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => {
                        dump(regs)
                        (-1, regs)
                    }
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                }
                case "GOTO" :: label :: Nil => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => {
                        labels.get(label) match {
                            case Some(v) => (v, regs)
                            case None => {
                                System.err.println("Error in GOTO: unkown label '" + label + "'")
                                (-2, regs)
                            }
                        }
                    }
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                }
                case "STORE" :: n :: Nil => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => (pc + 1, regs + (n.toInt -> regs.getOrElse(0, 0)))
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                }
                case "INDSTORE" :: n :: Nil => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => (pc + 1, regs + (regs.getOrElse(n.toInt, 0) -> regs.getOrElse(0, 0)))
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                } 
                case "IF" :: op :: n :: "GOTO" :: label :: Nil if List("<", ">", "=", "!=", "<=", ">=").contains(op) => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => {
                        val jump = op match {
                            case "<" => regs.getOrElse(0, 0) < n.toInt
                            case ">" => regs.getOrElse(0, 0) > n.toInt
                            case "<=" => regs.getOrElse(0, 0) <= n.toInt
                            case ">=" => regs.getOrElse(0, 0) >= n.toInt
                            case "=" => regs.getOrElse(0, 0) == n.toInt
                            case "!=" => regs.getOrElse(0, 0) != n.toInt
                        }
                        if (jump) {
                            labels.get(label) match {
                                case Some(v) => (v, regs)
                                case None => {
                                    System.err.println("Error in IF: unkown label '" + label + "'")
                                    (-2, regs)
                                }
                            }
                        } else {
                            (pc + 1, regs)
                        }
                    }
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                }
                case "LOAD" :: n :: Nil => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => (pc + 1, regs + (0 -> regs.getOrElse(n.toInt, 0)))
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                }
                case "CLOAD" :: n :: Nil => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => (pc + 1, regs + (0 -> n.toInt))
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                }
                case "INDLOAD" :: n :: Nil => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => (pc + 1, regs + (0 -> regs.getOrElse(regs.getOrElse(n.toInt, 0), 0)))
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                }
                case "ADD" :: n :: Nil => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => (pc + 1, regs + (0 -> (regs.getOrElse(0, 0) + regs.getOrElse(n.toInt, 0))))
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                }
                case "CADD" :: n :: Nil => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => (pc + 1, regs + (0 -> (regs.getOrElse(0, 0) + n.toInt)))
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                }
                case "INDADD" :: n :: Nil => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => (pc + 1, regs + (0 -> (regs.getOrElse(0, 0) + regs.getOrElse(regs.getOrElse(n.toInt, 0), 0))))
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                }
                case "SUB" :: n :: Nil => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => (pc + 1, regs + (0 -> (regs.getOrElse(0, 0) - regs.getOrElse(n.toInt, 0))))
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                }
                case "CSUB" :: n :: Nil => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => (pc + 1, regs + (0 -> (regs.getOrElse(0, 0) - n.toInt)))
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                }
                case "INDSUB" :: n :: Nil => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => (pc + 1, regs + (0 -> (regs.getOrElse(0, 0) - regs.getOrElse(regs.getOrElse(n.toInt, 0), 0))))
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                }
                case "MUL" :: n :: Nil => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => (pc + 1, regs + (0 -> (regs.getOrElse(0, 0) * regs.getOrElse(n.toInt, 0))))
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                }
                case "CMUL" :: n :: Nil => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => (pc + 1, regs + (0 -> (regs.getOrElse(0, 0) * n.toInt)))
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                }
                case "INDMUL" :: n :: Nil => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => (pc + 1, regs + (0 -> (regs.getOrElse(0, 0) * regs.getOrElse(regs.getOrElse(n.toInt, 0), 0))))
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                }
                case "DIV" :: n :: Nil => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => (pc + 1, regs + (0 -> (regs.getOrElse(0, 0) / regs.getOrElse(n.toInt, 0))))
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                }
                case "CDIV" :: n :: Nil => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => (pc + 1, regs + (0 -> (regs.getOrElse(0, 0) / n.toInt)))
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                }
                case "INDDIV" :: n :: Nil => {
                    val instruction = (pc: PC, regs: Registers, labels: Labels) => (pc + 1, regs + (0 -> (regs.getOrElse(0, 0) / regs.getOrElse(regs.getOrElse(n.toInt, 0), 0))))
                    parse(icount + 1, instructions :+ instruction, labels, lines)(Nil)
                }
                case tokens => {
                    Left("Syntax Error: icount=" + icount + "; tokens=" + tokens + ";")
                }
            }
        }

        parse(0, Vector(), Map(), lines)(Nil) match {
            case Left(message) => Left(message)
            case Right((_, instructions, labels, _)) => Right(instructions, labels)
        }
    }

    def run(instructions: Instructions, labels: Labels, regs: Registers): Boolean = {
        def run: (PC, Registers) => Boolean = {
            case (pc, regs) if pc >= 0 && pc < instructions.length => {
                val (newpc, newregs) = instructions(pc)(pc, regs, labels)
                run(newpc, newregs)
            }
            case (-1, regs) => true
            case (_, regs) => {
                System.err.println("RAM did not exit using 'END'")
                dump(regs)
                false
            }
        }

        run(labels.getOrElse("_start", 0), regs)
    }

}

object Ramsim extends App {

    if (args.length == 0) {
        println("usage: scala ramsim.scala <ramfile> [c(1), c(2), c(3), ...]")
        println("Sntax:")
        println("\t<label>:")
        println("\tSTORE       <n>")
        println("\t[C|IND]LOAD <n>")
        println("\t[C|IND]ADD  <n>")
        println("\t[C|IND]SUB  <n>")
        println("\t[C|IND]MUL  <n>")
        println("\t[C|IND]DIV  <n>")
        println("\tDUMP        <n> # dump registers 0 to n")
        println("\tGOTO <label>")
        println("\tEND")
        println("\tIF [=|!=|<|>|<=|>=] <n> GOTO <label>")
        System.exit(1)
    }

    val filename = args(0)

    Ram.parse(Source.fromFile(filename).getLines.toStream) match {
        case Left(message) => {
            System.err.println(message)
            System.exit(1)
        }
        case Right((instructions, labels)) => {
            val regs = args.tail.foldLeft((1, Map[Int, Int]()))((t, arg: String) => (t._1 + 1, t._2 + (t._1 -> arg.toInt)))._2
            Ram.run(instructions, labels, regs)
        }
    }

}
