package Modules.JackAnalyzerClasses

import java.io.{File, FileWriter}

import JackAnalyzer.VarKind.VarKind
import JackAnalyzer.{SymbolTable, VarKind}
import Models.{Argument, Identifier}
import Modules.JackAnalyzerClasses.LabelKind.LabelKind
import Tools.Util

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class CompilationEngine {

  /* Private Fields */

  private var _tokenizer:JackTokenizer = _
  private var _outputFileUrl:String = _
  private var _currentIndentLevel = 0
  private var _symbolTable = new SymbolTable()

  private var _outputFileName:String = _
  private var _isXml = false
  private var _kindMap = Map(
    "static" -> VarKind.STATIC,
    "field" -> VarKind.FIELD,
  )
  private val _labelsCount:mutable.HashMap[LabelKind, Int] = mutable.HashMap(
    LabelKind.IF_TRUE -> 0,
    LabelKind.IF_FALSE -> 0,
    LabelKind.IF_END -> 0,
    LabelKind.WHILE_EXP -> 0,
    LabelKind.WHILE_END -> 0,
  )

  private var _cmdsCount = 0


  /* Constructor */

  def this(tokenizer:JackTokenizer, outputFileUrl:String){
    this()

    // Creates a new compilation engine with the given input and output.
    // The next routine called must be compileClass

    _tokenizer = tokenizer
    _outputFileUrl = outputFileUrl

    val f = new File(_outputFileUrl)
    _outputFileName = f.getName.replace(".jack", "")
                               .replace(".vm", "")
  }


  /* Compile Methods */

  /* DONE
   * Compiles a complete class.
   * "class" className '{' classVarDec* subroutineDec* '}'
   */
  def compileClass():Unit ={
    if (_tokenizer.hasMoreTokens()){
      _tokenizer.advance()

      // "class"
      if (_tokenizer.tokenType() == TokenType.KEYWORD && _tokenizer.keyword() == "class"){

        val tag = "class"
        writeOpeningTag(tag)

        eatCurrentToken()

        // className
        if (isIdentifier(_tokenizer.getCurrentToken())){
          eatCurrentToken()

          // '{'
          if (isSymbolEqual(_tokenizer.getCurrentToken(), "{")){
            eatCurrentToken()

            // classVarDec*
            while(compileClassVarDec()){}

            // subroutineDec*
            while(compileSubroutineDec()){}

            // '}'
            if (isSymbolEqual(_tokenizer.getCurrentToken(), "}")){
              eatCurrentToken()

              writeClosingTag(tag)

              return None
            }
          }
        }

        writeClosingTag(tag)
      }
    }
  }

  /* DONE
   * Compiles a static variable declaration, or a field deceleration.
   * ("static" | "field") type varName(',' varName)* ';'
   */
  def compileClassVarDec():Boolean ={
    if (_tokenizer.hasMoreTokens()){

      val varDecs = List("static", "field")

      // ("static" | "field")
      if (_tokenizer.tokenType() == TokenType.KEYWORD &&
        varDecs.contains(_tokenizer.keyword())){

        val tag = "classVarDec"
        writeOpeningTag(tag)

        //*EDIT
        var varKind = _kindMap(_tokenizer.keyword())
        eatCurrentToken()

        // type
        if (isType(_tokenizer.getCurrentToken())){
          //*EDIT
          var varType = _tokenizer.getCurrentTokenValue()
          eatCurrentToken()

          // varName
          if (isIdentifier(_tokenizer.getCurrentToken())) {
            //*EDIT
            var varName = _tokenizer.identifier()
            _symbolTable.define(varName, varType, varKind)
            eatCurrentToken()

            // (',' varName)*
            while(isSymbolEqual(_tokenizer.getCurrentToken(), ",")){
              eatCurrentToken()// eat ','

              // varName
              if (isIdentifier(_tokenizer.getCurrentToken())){
                //*EDIT
                varName = _tokenizer.identifier()
                _symbolTable.define(varName, varType, varKind)
                eatCurrentToken()// eat varName
              }
            }

            // ';'
            if (isSymbolEqual(_tokenizer.getCurrentToken(), ";")){
              eatCurrentToken()

              writeClosingTag(tag)

              return true
            }
          }
        }

        writeClosingTag(tag)
      }
    }

    return false
  }

  /* DONE
   * Compiles a complete method, function, or constructor.
   * ("constructor" | "function" | "method")("void" | type) subroutineName
   * '(' parameterList ')' subroutineBody
   */
  def compileSubroutineDec():Boolean ={
    if (_tokenizer.hasMoreTokens()){
      val subroutineDecs = List("constructor", "function", "method")

      // ("constructor" | "function" | "method")
      if (_tokenizer.tokenType() == TokenType.KEYWORD &&
        subroutineDecs.contains(_tokenizer.keyword())){

        val tag = "subroutineDec"
        writeOpeningTag(tag)

        //*EDIT
        val subKind = _tokenizer.getCurrentTokenValue()
        resetSubroutine()

        eatCurrentToken()

        // ("void" | type)
        if (isKeywordEqual(_tokenizer.getCurrentToken(), "void") || isType(_tokenizer.getCurrentToken())){

          //*EDIT
          val returnType = _tokenizer.getCurrentTokenValue()

          eatCurrentToken()

          // subroutineName
          if (isIdentifier(_tokenizer.getCurrentToken())){
            //*EDIT
            val subName = _tokenizer.identifier()

            eatCurrentToken()

            // '('
            if (isSymbolEqual(_tokenizer.getCurrentToken(), "(")){
              eatCurrentToken()

              if (subKind == "method"){
                _symbolTable.define("this", _outputFileName, VarKind.ARG)
              }

              // parameterList
              compileParameterList()

              //*EDIT
              val paramsCount = _symbolTable.getVarCount(VarKind.ARG)

              // ')'
              if (isSymbolEqual(_tokenizer.getCurrentToken(), ")")){

                //*EDIT
                val fullSubName = generateFullSubName(subName)

                eatCurrentToken()

                // subroutineBody
                if (compileSubroutineBody(subKind, fullSubName)){
                  writeClosingTag(tag)

                  return true
                }
              }
            }
          }
        }

        writeClosingTag(tag)
      }
    }

    return false;
  }

  /* DONE
   * Compiles a (possibly empty) parameter list.
   * Doesn't handle the enclosing "()".
   * ((type varName) (',' type varName)*)?
   */
  def compileParameterList():Unit ={
    val tag = "parameterList"
    writeOpeningTag(tag)

    // type
    if (isType(_tokenizer.getCurrentToken())){
      //*EDIT
      var varType = _tokenizer.getCurrentToken().tokenValue

      eatCurrentToken()

      // varName
      if (isIdentifier(_tokenizer.getCurrentToken())){
        //*EDIT
        var varName = _tokenizer.getCurrentToken().tokenValue
        _symbolTable.define(varName, varType, VarKind.ARG)

        eatCurrentToken()

        // (',' type varName)*
        while (isSymbolEqual(_tokenizer.getCurrentToken(), ",")){
          eatCurrentToken() // ','

          // type
          if (isType(_tokenizer.getCurrentToken())){
            //*EDIT
            var varType = _tokenizer.getCurrentToken().tokenValue

            eatCurrentToken()

            // varName
            if (isIdentifier(_tokenizer.getCurrentToken())){
              //*EDIT
              var varName = _tokenizer.getCurrentToken().tokenValue
              _symbolTable.define(varName, varType, VarKind.ARG)

              eatCurrentToken()
            }
            else{
              // TODO: throw an error? expected varName
            }
          }
          else{
            // TODO: throw an error? expected type
          }
        }
      }

    }

    writeClosingTag(tag)
  }

  /* DONE
   * Compiles a subroutine's body
   * '{' varDec* statements '}'
   */
  def compileSubroutineBody(subKind:String, fullSubName:String):Boolean ={
    if (_tokenizer.hasMoreTokens()){//TODO:Del this "if" statement?

      // '{'
      if (isSymbolEqual(_tokenizer.getCurrentToken(), "{")){
        val tag = "subroutineBody"
        writeOpeningTag(tag)

        eatCurrentToken()

        // varDec*
        while (compileVarDec()){}

        //*EDIT
        val varsCount = _symbolTable.getVarCount(VarKind.VAR)

        //*EDIT
        writeVmCommand("function " + fullSubName + " " + varsCount)
        if (subKind == "constructor"){
          val fieldsCount = _symbolTable.getVarCount(VarKind.FIELD)
          writePushConst(fieldsCount)
          writeVmCommand("call Memory.alloc 1")
          writeVmCommand("pop pointer 0")
        }
        else if (subKind == "method"){
          writePush("this")
          writeVmCommand("pop pointer 0")
        }


        // statements
        if (compileStatements()){

          // '}'
          if (isSymbolEqual(_tokenizer.getCurrentToken(), "}")){
            eatCurrentToken()

            writeClosingTag(tag)

            return true
          }
        }

        writeClosingTag(tag)
      }
    }

    return false
  }

  /* DONE
   * Compiles a var declaration.
   * "var" type varName (',' varName)* ';'
   */
  def compileVarDec():Boolean ={
    if (_tokenizer.hasMoreTokens()){//TODO: Del this "if" statement?

      // "var"
      if (_tokenizer.tokenType() == TokenType.KEYWORD &&
        _tokenizer.keyword() == "var"){

        val tag = "varDec"
        writeOpeningTag(tag)

        eatCurrentToken()

        // type
        if (isType(_tokenizer.getCurrentToken())){
          //*EDIT
          var varType = _tokenizer.getCurrentTokenValue()
          eatCurrentToken()

          // varName
          if (isIdentifier(_tokenizer.getCurrentToken())){
            //*EDIT
            var varName = _tokenizer.getCurrentTokenValue()
            _symbolTable.define(varName, varType, VarKind.VAR)
            eatCurrentToken()

            // (',' varName)*
            while (isSymbolEqual(_tokenizer.getCurrentToken(), ",")){
              eatCurrentToken() // eat ','

              // varName
              if (isIdentifier(_tokenizer.getCurrentToken())){
                //*EDIT
                varName = _tokenizer.getCurrentTokenValue()
                _symbolTable.define(varName, varType, VarKind.VAR)
                eatCurrentToken()
              }
            }

            // ';'
            if (isSymbolEqual(_tokenizer.getCurrentToken(), ";")){
              eatCurrentToken()

              writeClosingTag(tag)

              return true
            }
          }
        }

        writeClosingTag(tag)
      }
    }

    return false
  }

  /* DONE
   * Compiles a sequence of statements.
   * Doesn't handle the enclosing "{}"
   */
  def compileStatements():Boolean ={
    // TODO: always returns true, change to Unit?
    val tag = "statements"
    writeOpeningTag(tag)

    while (compileStatement()){}

    writeClosingTag(tag)
    return true
  }

  /*
   * Compiles a let statement
   * "let" varName ('['expression']')? '=' expression ';'
   */
  def compileLet():Boolean ={
    if (_tokenizer.hasMoreTokens()){//TODO:Delete this "if" statement?

      // "let"
      if (isKeywordEqual(_tokenizer.getCurrentToken(), "let")){

        val tag = "letStatement"
        writeOpeningTag(tag)

        eatCurrentToken()

        // varName
        if (isIdentifier(_tokenizer.getCurrentToken())){
          //*EDIT
          val varName = _tokenizer.getCurrentTokenValue()
          var isAssignArr = false

          eatCurrentToken()

          //('[' expression ']')?
          if (isSymbolEqual(_tokenizer.getCurrentToken(), "[")){
            eatCurrentToken() // eat '['

            // expression
            if (compileExpression()){

              // ']'
              if (isSymbolEqual(_tokenizer.getCurrentToken() ,"]")){
                //*EDIT
                writePush(varName)
                writeAdd()
                isAssignArr = true

                eatCurrentToken()
              }
              else{
                // TODO: throw? expected ']'
              }
            }
            else {
              // TODO: throw? expected expression
            }
          }

          // '='
          if (isSymbolEqual(_tokenizer.getCurrentToken(), "=")){
            eatCurrentToken()

            // expression
            if (compileExpression()){

              // ';'
              if (isSymbolEqual(_tokenizer.getCurrentToken(), ";")){
                //*EDIT
                if (isAssignArr){
                  writeVmCommand("pop temp 0") // save in temp
                  writeVmCommand("pop pointer 1") // pop THAT
                  writeVmCommand("push temp 0")
                  writeVmCommand("pop that 0")
                }
                else{
                  val command = "pop " + _symbolTable.kindOf(varName).toString + " " + _symbolTable.indexOf(varName).toString
                  writeVmCommand(command)
                }
                eatCurrentToken()

                writeClosingTag(tag)

                return true
              }
            }
          }
        }

        writeClosingTag(tag)
      }
    }

    return false
  }

  /* DONE
   * Compiles an if statement, possibly with a trailing else clause
   * 'if' '(' expression ')' '{' statements '}' ("else" '{' statements '}')?
   */
  def compileIf():Boolean ={

    if (_tokenizer.hasMoreTokens()){

      // if
      if (isKeywordEqual(_tokenizer.getCurrentToken(), "if")){
        val tag = "ifStatement"
        writeOpeningTag(tag)

        eatCurrentToken()

        // '('
        if (isSymbolEqual(_tokenizer.getCurrentToken(), "(")){
          eatCurrentToken()

          // expression
          if (compileExpression()){

            // ')'
            if (isSymbolEqual(_tokenizer.getCurrentToken(), ")")){

              //region *EDIT

              val ifTrueLabel = generateLabel(LabelKind.IF_TRUE)
              val ifFalseLabel = generateLabel(LabelKind.IF_FALSE)
              val ifEndLabel = generateLabel(LabelKind.IF_END)
              //can also be implemented by "not" and then "if-goto IF_TRUE"
              writeVmCommand("if-goto " + ifTrueLabel) // if-goto L1
              writeVmCommand("goto " + ifFalseLabel)
              writeLabel(ifTrueLabel)

              //endregion

              eatCurrentToken()

              // '{'
              if (isSymbolEqual(_tokenizer.getCurrentToken(), "{")){
                eatCurrentToken()

                // statements
                if (compileStatements()){

                  // '}'
                  if (isSymbolEqual(_tokenizer.getCurrentToken(), "}")){
                    eatCurrentToken()

                    // ("else" '{' statements '}')?
                    if (isKeywordEqual(_tokenizer.getCurrentToken(), "else")){
                      //*EDIT
                      writeVmCommand("goto " + ifEndLabel)
                      writeLabel(ifFalseLabel)
                      eatCurrentToken()

                      // '{'
                      if (isSymbolEqual(_tokenizer.getCurrentToken(), "{")){
                        eatCurrentToken()

                        // statements
                        if (compileStatements()){

                          // '}'
                          if (isSymbolEqual(_tokenizer.getCurrentToken(), "}")){
                            //*EDIT
                            writeLabel(ifEndLabel)
                            eatCurrentToken()

                            writeClosingTag(tag)

                            return true
                          }
                          else{
                            writeClosingTag(tag) // TODO: throw? expected '}'

                            return false
                          }
                        }
                        else{
                          writeClosingTag(tag) // TODO: throw? expected statements

                          return false
                        }
                      }
                      else{
                        writeClosingTag(tag) // TODO: throw? expected '{'

                        return false
                      }
                    }
                    else{
                      //*EDIT
                      writeLabel(ifFalseLabel)

                      writeClosingTag(tag)

                      return true
                    }
                  }
                }
              }
            }
          }
        }

        writeClosingTag(tag)
      }
    }

    return false
  }

  /* DONE
   * Compiles a while statement.
   * "while" '(' expression ')' '{' statements '}'
   */
  def compileWhile():Boolean ={
    if (_tokenizer.hasMoreTokens()){

      // "while"
      if (isKeywordEqual(_tokenizer.getCurrentToken(), "while")){
        val tag = "whileStatement"
        writeOpeningTag(tag)

        //*EDIT
        val whileLabel = generateLabel(LabelKind.WHILE_EXP)
        val whileEndLabel = generateLabel(LabelKind.WHILE_END)
        writeLabel(whileLabel) // label L1
        eatCurrentToken()

        if (isSymbolEqual(_tokenizer.getCurrentToken(), "(")){
          eatCurrentToken()

          // expression
          if (compileExpression()){

            // ')'
            if (isSymbolEqual(_tokenizer.getCurrentToken(), ")")){
              //*EDIT
              writeVmCommand("not") // not
              writeVmCommand("if-goto " + whileEndLabel) // if-goto L2
              eatCurrentToken()

              // '{'
              if (isSymbolEqual(_tokenizer.getCurrentToken(), "{")){
                eatCurrentToken()

                // statements
                if (compileStatements()){

                  // '}'
                  if (isSymbolEqual(_tokenizer.getCurrentToken(), "}")){
                    //*EDIT
                    writeVmCommand("goto " + whileLabel) // goto L1
                    writeLabel(whileEndLabel) // label L2
                    eatCurrentToken()

                    writeClosingTag(tag)

                    return true
                  }
                }
              }
            }
          }
        }

        writeClosingTag(tag)
      }
    }

    return false
  }

  /* DONE
   * Compiles a do statement
   * "do" subroutineCall ';'
   */
  def compileDo():Boolean ={
    if (_tokenizer.hasMoreTokens()){//TODO: delete this "if" statement?

      // "do"
      if (isKeywordEqual(_tokenizer.getCurrentToken(), "do")){
        val tag = "doStatement"
        writeOpeningTag(tag)

        eatCurrentToken()

        // subroutineCall
        if (compileSubroutineCall()){

          // ';'
          if (isSymbolEqual(_tokenizer.getCurrentToken(), ";")){
            eatCurrentToken()

            //*EDIT
            writeVmCommand("pop temp 0")

            writeClosingTag(tag)

            return true
          }
        }

        writeClosingTag(tag)
      }
    }

    return false
  }

  /* DONE
   * Compiles a return statement.
   * "return" expression? ';'
   */
  def compileReturn():Boolean ={
    if (_tokenizer.hasMoreTokens()){

      // "return"
      if (isKeywordEqual(_tokenizer.getCurrentToken(), "return")){
        val tag = "returnStatement"
        writeOpeningTag(tag)

        eatCurrentToken()

        // expression?
        if (!compileExpression()){
          writePushConst(0)
        }

        // ';'
        if (isSymbolEqual(_tokenizer.getCurrentToken(), ";")){
          //region *EDIT

          writeVmCommand("return") // return

          //endregion

          eatCurrentToken()

          writeClosingTag(tag)

          return true
        }

        writeClosingTag(tag)
      }
    }

    return false
  }

  /* DONE
   * Compiles an expression
   * term (op term)*
   */
  def compileExpression():Boolean ={
    val tag = "expression"

    // term (op term)*
    if (isStartOfTerm()){
      writeOpeningTag(tag)

      // term
      if (compileTerm()){

        // (op term)*
        //region while (isOp(_tokenizer.getCurrentToken())){...}
        while (isOp(_tokenizer.getCurrentToken())){
          //*EDIT
          val op = _tokenizer.getCurrentTokenValue()
          eatCurrentToken()

          // term
          if (!compileTerm()){
            writeClosingTag(tag)

            // TODO: throw? expected term
            return false
          }

          //*EDIT
          writeOp(op)
        }
        //endregion

        writeClosingTag(tag)

        return true
      }



      writeClosingTag(tag)

      return false
    }

    return false
  }

  /* DONE
   * Compiles a term.
   * If the current token is an IDENTIFIER,
   * the routine must distinguish between a variable, an array entry, or a subroutine call.
   * A single look-ahead token,
   * which may be one of "[", "(", or ".", suffices to distinguish between possibilities.
   * Any other token isn't part of this term and should not be advanced over.
   *
   * integerConstant | stringConstant | keywordConstant | varName |
   * varName'['expression']' | subroutineCall | '('expression')' | unaryOp term
   */
  def compileTerm():Boolean ={
    val tag = "term"

    val currentToken = _tokenizer.getCurrentToken()
    val nextToken = _tokenizer.getNextToken()

    /*    integerConstant | stringConstant | keywordConstant    */ //DONE
    if (isIntConst(currentToken) || isStringConst(currentToken) || isKeywordConst(currentToken)){
      writeOpeningTag(tag)

      //region *EDIT

      val currTokValue = currentToken.tokenValue
      //number
      if (isIntConst(currentToken)){
        writePushConst(currTokValue)
      }
      else if (isKeywordConst(currentToken)){
        if (currTokValue == "null" || currTokValue == "false"){
          writePushConst(0)
        }
        else if (currTokValue == "true"){
          // can also be implemented by "push 1" and then "neg"
          writePushConst(0)
          writeVmCommand("not")
        }
        else if (currTokValue == "this"){
          writeVmCommand("push pointer 0")
        }
      }
      else if (isStringConst(currentToken)){
        val str = currTokValue

        writePushConst(str.length)
        writeVmCommand("call String.new 1")

        str.foreach(c => {
          writePushConst(c.toInt)
          writeVmCommand("call String.appendChar 2")
        })
      }
      //endregion

      eatCurrentToken()

      writeClosingTag(tag)

      return true
    }
    /*    varName'['expression']' <=> identifier'[' etc...    */ //DONE
    else if (isIdentifier(currentToken) && isSymbolEqual(nextToken, "[")){
      writeOpeningTag(tag)

      //*EDIT
      val varName = currentToken.tokenValue

      eatCurrentToken() // eat varName
      eatCurrentToken() // eat '['

      // expression
      if (compileExpression()){

        // ']'
        if (isSymbolEqual(_tokenizer.getCurrentToken(), "]")){
          //EDIT*

          writePush(varName)
          writeAdd()
          writeVmCommand("pop pointer 1")
          writeVmCommand("push that 0")
          eatCurrentToken()

          writeClosingTag(tag)

          return true
        }
      }

      writeClosingTag(tag)

      // TODO: throw? expected expression']'
      return false
    }
    /*  subroutineCall => (className|varName)'.'subroutineName'('expressionList')'  */ //DONE
    else if(isIdentifier(currentToken) && isSymbolEqual(nextToken, ".")){
      writeOpeningTag(tag)

      if (compileSubroutineCall()){
        writeClosingTag(tag)

        return true
      }

      writeClosingTag(tag)

      return false
    }
    /* subroutineCall =>  subroutineName'('expressionList')'  */
    else if(isIdentifier(currentToken) && isSymbolEqual(nextToken, "(")){
      writeOpeningTag(tag)

      if (compileSubroutineCall()){
        writeClosingTag(tag)

        return  true
      }

      writeClosingTag(tag)

      return false
    }
    // varName
    else if (isIdentifier(currentToken)){
      writeOpeningTag(tag)

      //*EDIT
      val varName = currentToken.tokenValue
      writePush(varName)

      eatCurrentToken()

      writeClosingTag(tag)

      return true
    }
    // '('expression')'
    else if (isSymbolEqual(currentToken, "(")){
      writeOpeningTag(tag)

      eatCurrentToken() // eat '('

      // expression
      if (compileExpression()){

        // ')'
        if (isSymbolEqual(_tokenizer.getCurrentToken(), ")")){
          eatCurrentToken()

          writeClosingTag(tag)

          return true
        }
      }

      writeClosingTag(tag)

      return false
    }
    // unaryOp term
    else if (isUnaryOp(currentToken)){
      writeOpeningTag(tag)

      // *EDIT
      val unaryOp = currentToken.tokenValue

      eatCurrentToken()

      // term
      if (compileTerm()){
        writeClosingTag(tag)

        //*EDIT
        writeUnaryOp(unaryOp)

        return true
      }

      writeClosingTag(tag)

      return false
    }

    return false
  }

  /* DONE
   * Compiles a (possibly empty) comma-separated list of expressions.
   * (expression (',' expression)* )?
   */
  def compileExpressionList():Int ={
    val tag = "expressionList"
    writeOpeningTag(tag)

    //*EDIT
    var expCount = 0

    // expression
    if (compileExpression()){
      //*EDIT
      expCount += 1

      // (',' expression)*
      //region while (isSymbolEqual(_tokenizer.getCurrentToken(), ",")){ ... }
      while (isSymbolEqual(_tokenizer.getCurrentToken(), ",")){
        eatCurrentToken() // eat ','

        if (!compileExpression()){
          writeClosingTag(tag)

          // TODO: throw? expected expression after ','
          return -1
        }

        //*EDIT
        expCount += 1
      }
      //endregion
    }

    writeClosingTag(tag)

    return expCount
  }

  /* DONE
   * Compiles a single statement
   * letStatement | ifStatement | whileStatement | doStatement | returnStatement
   */
  def compileStatement():Boolean ={
    return compileLet() ||
      compileIf() ||
      compileWhile() ||
      compileDo() ||
      compileReturn()
  }

  /* DONE
   * Compiles a subroutineCall.
   * subroutineName '(' expressionList ')' |
   * (className | varName)'.' subroutineName '(' expressionList ')'
   *                              <=>
   * identifier ('('expressionList')' | '.'subroutineName'('expressionList')')
   */
  private def compileSubroutineCall():Boolean ={

    // subroutineName | className | varName <=> identifier
    if (isIdentifier(_tokenizer.getCurrentToken())){

      //region *EDIT

      val identifier = _tokenizer.getCurrentTokenValue()
      var argsCount = 0

      //endregion

      eatCurrentToken() // eat identifier

      // '(' => subroutineName '(' expressionList')'
      if (isSymbolEqual(_tokenizer.getCurrentToken(), "(")){
        eatCurrentToken()

        //region *EDIT
        writeVmCommand("push pointer 0")
        argsCount += compileExpressionList()
        argsCount += 1

        //endregion

        // expressionList
        if (argsCount != -1){

          // ')'
          if (isSymbolEqual(_tokenizer.getCurrentToken(), ")")){
            //*EDIT
            writeCall(generateFullSubName(identifier), argsCount)

            eatCurrentToken()

            return true
          }
        }
      }
      // '.' => (className|varName) '.' subroutineName'('expressionList')'
      else if (isSymbolEqual(_tokenizer.getCurrentToken(), ".")){
        eatCurrentToken()

        // subroutineName
        if (isIdentifier(_tokenizer.getCurrentToken())){

          //region *EDIT
          val isVar = isVarName(identifier)
          if (isVar){
            writePush(identifier)
            argsCount += 1
          }
          val subName = _tokenizer.getCurrentTokenValue()

          //endregion

          eatCurrentToken()

          // '('
          if (isSymbolEqual(_tokenizer.getCurrentToken(), "(")){
            eatCurrentToken()

            // expressionList
            //*EDIT
            argsCount += compileExpressionList()

            if (argsCount >= 0){

              // ')'
              if (isSymbolEqual(_tokenizer.getCurrentToken(), ")")){

                //region *EDIT

                if(isVar){
                  writeCall(_symbolTable.typeOf(identifier) + "." + subName, argsCount)
                }
                else{
                  writeCall(identifier + "." + subName, argsCount)
                }

                //endregion

                eatCurrentToken()

                return true
              }
            }
          }
        }
      }
    }

    return false
  }


  /* Private Helper Methods */

  /*
   * Writes the current token to the output file, and advances the tokenizer.
   */
  private def eatCurrentToken(): Unit ={

    //*EDIT
    if (!_isXml){
      _tokenizer.advance()

      return
    }

    val writer = new FileWriter(_outputFileUrl, true)

    var i = 0;
    while (i < _currentIndentLevel * 2){
      writer.write(' ')

      i += 1
    }

    writer.write(_tokenizer.getCurrentToken().toXML() + '\n')

    writer.close()

    _tokenizer.advance()
  }

  private def isType(token:Token):Boolean ={
    val tokenType = token.tokenType
    val tokenValue = token.tokenValue
    val types = List("int", "char", "boolean")

    return (tokenType == TokenType.KEYWORD && types.contains(tokenValue)) ||
      tokenType == TokenType.IDENTIFIER
  }

  private def isIdentifier(token:Token):Boolean ={
    val pattern = "^[a-zA-Z_][\\w_]*".r

    return token.tokenType == TokenType.IDENTIFIER &&
      pattern.matches(token.tokenValue)
  }

  private def isSymbolEqual(token:Token, symbol:String): Boolean ={
    return isSymbol(token) &&
      token.tokenValue == symbol
  }

  private def isKeywordEqual(token:Token, keyword:String): Boolean ={
    return isKeyword(token) &&
      token.tokenValue == keyword
  }

  private def isKeyword(token:Token):Boolean ={
    val keywords:List[String] = List(
      "class", "constructor", "function", "method", "field", "static",
      "var", "int", "char", "boolean", "void", "true", "false",
      "null", "this", "let", "do", "if", "else", "while", "return"
    )

    return token.tokenType == TokenType.KEYWORD &&
      keywords.contains(token.tokenValue)
  }

  private def isSymbol(token:Token):Boolean ={
    val symbolsMap:Map[Char, String] = Map(
      '{' -> "{",
      '}' -> "}",
      '(' -> "(",
      ')' -> ")",
      '[' -> "[",
      ']' -> "]",
      '.' -> ".",
      ',' -> ",",
      ';' -> ";",
      '+' -> "+",
      '-' -> "-",
      '*' -> "*",
      '/' -> "/",
      '&' -> "&amp;",
      '|' -> "|",
      '<' -> "&lt;",
      '>' -> "&gt;",
      '=' -> "=",
      '~' -> "~",
    )

    return token.tokenType == TokenType.SYMBOL &&
      symbolsMap.values.toList.contains(token.tokenValue)
  }

  private def isIntConst(token:Token): Boolean ={
    val tokenType = token.tokenType
    val tokenValue = token.tokenValue

    return tokenType == TokenType.INT_CONST &&
      Util.isInt(tokenValue) &&
      tokenValue.toInt <= 32767 &&
      tokenValue.toInt >= 0
  }

  private def isStringConst(token:Token):Boolean ={
    val tokenType = token.tokenType
    val tokenValue = token.tokenValue
    val pattern = "[^\n\"]*".r

    return tokenType == TokenType.STRING_CONST &&
      pattern.matches(tokenValue)
  }

  private def isKeywordConst(token:Token):Boolean ={
    // TODO: Refactor all lists to static class?
    val keywords:List[String] = List("true", "false", "null", "this")

    return token.tokenType == TokenType.KEYWORD &&
      keywords.contains(token.tokenValue)
  }

  private def isOp(token:Token): Boolean ={
    val ops = List("+", "-", "*", "/", "&amp;", "|", "&lt;", "&gt;", "=")

    return token.tokenType == TokenType.SYMBOL &&
      ops.contains(token.tokenValue)
  }

  private def isUnaryOp(token:Token): Boolean ={
    val unaryOps = List("-", "~")

    return token.tokenType == TokenType.SYMBOL &&
      token.tokenValue.length == 1 &&
      unaryOps.contains(token.tokenValue)
  }

  private def isStartOfTerm():Boolean ={

    val currentToken = _tokenizer.getCurrentToken()
    val nextToken = _tokenizer.getNextToken()

    /*    integerConstant | stringConstant | keywordConstant    */
    if (isIntConst(currentToken) || isStringConst(currentToken) || isKeywordConst(currentToken)){

      return true
    }
    /*    varName'['expression']' <=> identifier'[' etc...    */
    else if (isIdentifier(currentToken) &&
      isSymbolEqual(nextToken, "[")){

      return true
    }
    /*    subroutineCall <=> identifier'.' etc..    */
    else if(isIdentifier(currentToken) && isSymbolEqual(nextToken, ".")){

      return true
    }
    // varName
    else if (isIdentifier(currentToken)){

      return true
    }
    // '('expression')'
    else if (isSymbolEqual(currentToken, "(")){

      return true
    }
    // unaryOp term
    else if (isUnaryOp(currentToken)){

      return true
    }

    return false
  }

  private def writeOpeningTag(str:String): Unit ={

    //*EDIT
    if (!_isXml){
      return
    }

    val writer = new FileWriter(_outputFileUrl, true)

    var i = 0
    while(i < _currentIndentLevel * 2){
      writer.write(' ')

      i += 1
    }

    writer.write("<" + str + ">\n")
    writer.close()

    _currentIndentLevel += 1
  }

  private def writeClosingTag(str:String): Unit ={

    //*EDIT
    if (!_isXml){
      return
    }

    _currentIndentLevel -= 1

    val writer = new FileWriter(_outputFileUrl, true)

    var i = 0
    while(i < _currentIndentLevel * 2){
      writer.write(' ')

      i += 1
    }

    writer.write("</" + str + ">\n")
    writer.close()

  }

  private def writeVmCommand(command:String): Unit ={
    val writer = new FileWriter(_outputFileUrl, true)

    //TODO: Delete before deploy
    _cmdsCount += 1
    if (_cmdsCount == 37 && _outputFileName.endsWith("Main")){
      val x = 5
    }


    writer.write(command + '\n')

    writer.close()
  }

  private def writeLabel(labelName:String): Unit ={
    writeVmCommand("label " + labelName)
  }

  private def writePush(identifier:String): Unit ={
    if (_symbolTable.kindOf(identifier) == VarKind.NONE){
      //TODO: throw?
      val x = 5
    }

    writeVmCommand("push " + _symbolTable.kindOf(identifier) + " " + _symbolTable.indexOf(identifier))
  }

  private def writePushConst(num:String): Unit ={
    writeVmCommand("push constant " + num.toString)
  }

  private def writePushConst(num:Int): Unit ={
    writeVmCommand("push constant " + num)
  }

  private def writeOp(op:String): Unit = {
    writeVmCommand(getVmOpCommand(op))
  }

  private def writeUnaryOp(unaryOp:String): Unit ={
    writeVmCommand(getVmUnaryOpCommand(unaryOp))
  }

  private def writeCall(subName:String, argsCount:Int): Unit ={
    writeVmCommand("call " + subName + " " + argsCount)
  }

  private def writeAdd(){
    writeVmCommand("add")
  }

  private def generateFullSubName(subName:String):String ={
    return _outputFileName + "." + subName
  }

  private def generateLabel(labelKind:LabelKind): String ={
    var result = labelKind.toString + _labelsCount(labelKind).toString

    _labelsCount(labelKind) += 1

    return result
  }

  private def getVmOpCommand(operator:String):String ={

    if (operator == "+") {
      return "add"
    }
    else if (operator == "-") {
      return "sub"
    }
    else if (operator == "*") {
      return "call Math.multiply 2"
    }
    else if (operator == "/") {
      return "call Math.divide 2"
    }
    else if (operator == "&amp;") {
      return "and"
    }
    else if (operator == "|") {
      return "or"
    }
    else if (operator == "&lt;") {
      return "lt"
    }
    else if (operator == "&gt;") {
      return "gt"
    }
    else if (operator == "=") {
      return "eq"
    }
    else {
      throw new AssertionError();
    }
  }

  private def getVmUnaryOpCommand(unaryOp:String):String ={

    if (unaryOp == "-"){
      return "neg"
    }

    //if "~"
    return "not"
  }

  private def isVarName(identifier:String):Boolean ={
    return _symbolTable.kindOf(identifier) != VarKind.NONE
  }

  private def resetSubroutine(): Unit ={
    _symbolTable.startSubroutine()
    LabelKind.values.foreach(labelKind => {
      _labelsCount(labelKind) = 0
    })
  }

}

object LabelKind extends Enumeration {
  type LabelKind = Value

  // Assigning values
  val IF_TRUE = Value("IF_TRUE")
  val IF_FALSE = Value("IF_FALSE")
  val IF_END = Value("IF_END")
  val WHILE_EXP = Value("WHILE_EXP")
  val WHILE_END = Value("WHILE_END")
}

