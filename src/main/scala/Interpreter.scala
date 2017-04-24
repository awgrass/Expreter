object Interpreter {

  //Type aliases to make the signatures more readable
  type FunctionName = String
  type VariableName = String

  var secMap: Map[String, Int] = Map()
  var refMap: Map[Int, Value] = Map()
  var refCounter:Int = 0

  def interpret(functionEnvironment: Map[FunctionName, FunctionDeclaration],
                variableEnvironment: Map[VariableName, Value],
                expression: Expression): Value = expression match {

    //TODO
    //This function should evaluate the given expression, using the functionEnvironment(delta)
    //and the variableEnvironment(omega).
    //
    //It might be helpful to define helper functions to evaluate different expressions,
    //like buildins or user defined functions.

    case condExp(p, e1, e2) => p.name match {
      case "eq" => interpret(functionEnvironment, variableEnvironment, eqCond(p.params, e1, e2))
      case "lt" => interpret(functionEnvironment, variableEnvironment, ltCond(p.params, e1, e2))
    }

    case eqCond(params, e1, e2) => params.length match {
      case 2 => (interpret(functionEnvironment, variableEnvironment, params.head),
        interpret(functionEnvironment, variableEnvironment, params.last)) match {
        case (intValue(a), intValue(b)) => a == b match {
          case true => interpret(functionEnvironment, variableEnvironment, e1)
          case false => interpret(functionEnvironment, variableEnvironment, e2)
        }
        case (listValue(a), listValue(b)) => a == b match {
          case true => interpret(functionEnvironment, variableEnvironment, e1)
          case false => interpret(functionEnvironment, variableEnvironment, e2)
        }
          case(refValue(a), refValue(b)) => a == b match {
            case true => interpret(functionEnvironment, variableEnvironment, e1)
            case false => interpret(functionEnvironment, variableEnvironment, e2)
          }
        case _ => interpret(functionEnvironment, variableEnvironment, e2)
      }
    }
    case ltCond(params, e1, e2) => params.length match {
      case 2 => (interpret(functionEnvironment, variableEnvironment, params.head),
        interpret(functionEnvironment, variableEnvironment, params.last)) match {
        case (intValue(a), intValue(b)) => a < b match {
          case true => interpret(functionEnvironment, variableEnvironment, e1)
          case false => interpret(functionEnvironment, variableEnvironment, e2)
        }
        case _ => interpret(functionEnvironment, variableEnvironment, e2)
      }
    }


    case functionExp(id, params) => id match {
      case "add" => params.length match {
        case 2 => interpret(functionEnvironment, variableEnvironment, add(params.head, params.last))
      }
      case "negate" => params.length match {
        case 1 => interpret(functionEnvironment, variableEnvironment, negate(params.head))
      }
      case "first" => params.length match {
        case 1 => interpret(functionEnvironment, variableEnvironment, first(params.head))
      }
      case "rest" => params.length match {
        case 1 => interpret(functionEnvironment, variableEnvironment, rest(params.head))
      }
      case "build" => params.length match {
        case 2 => interpret(functionEnvironment, variableEnvironment, build(params.head, params.last))
      }

      case _ => functionEnvironment.contains(id) match {
        case true => params.length == functionEnvironment(id).params.length match {
          case true => interpret(functionEnvironment, variableEnvironment, listExp(params)) match {
            case listValue(params) => params.isEmpty match {
              case true => interpret(functionEnvironment, variableEnvironment, functionEnvironment(id).body)
              case false => interpret(functionEnvironment, (functionEnvironment(id).params.zip(params)).toMap,
                functionEnvironment(id).body)
            }
            case _ => System.out.println("Function " + id + " not found !"); null


          }
          case false => System.out.println("Wrong parameter count for function " + id + "!"); null
        }
        case false => System.out.println("Function " + id + " not found ! "); null
      }
    }

    case add(a, b) => (interpret(functionEnvironment, variableEnvironment, a),
      interpret(functionEnvironment, variableEnvironment, b)) match {
      case (intValue(a), intValue(b)) => intValue(a + b)
      case _ => System.out.println("It's only possible adding integers !"); null
    }

    case negate(a) => interpret(functionEnvironment, variableEnvironment, a) match {
      case intValue(a) => intValue(a * (-1))
      case _ => System.out.println("It's only possible negating integers !"); null
    }

    case first(xs) => interpret(functionEnvironment, variableEnvironment, xs) match {
      case listValue(xs) => xs.head
      case _ => System.out.println("It's only possible applying first() to lists !"); null
    }

    case rest(xs) => interpret(functionEnvironment, variableEnvironment, xs) match {
      case listValue(xs) => listValue(xs.tail)
      case _ => System.out.println("It's only possible applying rest() to lists !"); null
    }

    case build(x, xs) => (interpret(functionEnvironment, variableEnvironment, x),
      interpret(functionEnvironment, variableEnvironment, xs)) match {
      case (intValue(x), listValue(xs)) => listValue(intValue(x) :: xs)
      case (listValue(x), listValue(xs)) => listValue(listValue(x) :: xs)
      case (refValue(x), listValue(xs)) => listValue(refValue(x) :: xs)
      case _ => System.out.println("build(x, y) ist only possible if x is Integer/List and y is List"); null
    }



    case intExp(i) => intValue(i)

    case listExp(l) => listValue(l.map(x => interpret(functionEnvironment, variableEnvironment, x)))




    case varExp(v) => variableEnvironment.contains(v) match {
      case true => variableEnvironment(v)
      case false => secMap.contains(v) match {
        case true => refValue(secMap(v))
        case false => System.out.println("There is no variable " + v + "!"); null

      }
    }

    case letExp(let, e) => let match {
      case Nil => interpret(functionEnvironment, variableEnvironment, e)
      case bindExp(id, v) :: xs => interpret(functionEnvironment, variableEnvironment ++ Map(id -> interpret(functionEnvironment, variableEnvironment, v)), letExp(xs, e));
      case refbindExp(id, v)::xs => val a = interpret(functionEnvironment, variableEnvironment, v);
                                    refMap += (refCounter -> a);
                                    secMap += (id -> refCounter);
                                    refCounter += 1;
                                    interpret(functionEnvironment, variableEnvironment, letExp(xs, e))
      case _ => System.out.println("Only val declaration possible in let bindings !"); null


    }

    case seqExp(l) => l match {
      case x::Nil => interpret(functionEnvironment, variableEnvironment, x);
      case assExp(id, e)::xs => secMap.contains(id) match {
        case true => refMap += (secMap(id) -> interpret(functionEnvironment, variableEnvironment, e)); 
          interpret(functionEnvironment, variableEnvironment, seqExp(xs))
        case false => System.out.println("There is no reference " + id + " to assign a value to !"); null
      }
      case x::xs => interpret(functionEnvironment, variableEnvironment, x); 
        interpret(functionEnvironment, variableEnvironment, seqExp(xs))
    }

    case derefExp(id) => refMap(secMap(id))

    case assExp(id, e) => secMap.contains(id) match {
      case true => refMap += (secMap(id) -> interpret(functionEnvironment, variableEnvironment, e));
                    interpret(functionEnvironment, variableEnvironment, e);
      case false => System.out.println("There is no reference " + id + " to assign a value to !"); null
    }

  }
}












