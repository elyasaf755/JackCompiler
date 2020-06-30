package Modules.JackAnalyzerClasses

import java.io.FileWriter
import Tools.Util

class CompilationEngine {

  /* Private Fields */

  private var _tokenizer:JackTokenizer = _
  private var _outputFileUrl:String = _
  private var _tabsCount = 0


  /* Constructor */

  def this(tokenizer:JackTokenizer, outputFileUrl:String){
    this()

    // Creates a new compilation engine with the given input and output.
    // The next routine called must be compileClass

    _tokenizer = tokenizer
    _outputFileUrl = outputFileUrl
  }


  /* Compile Methods */

  /*
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
    }//end of first "if" statement
  }

  /*
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

        eatCurrentToken()

        // type
        if (isType(_tokenizer.getCurrentToken())){
          eatCurrentToken()

          // varName
          if (isIdentifier(_tokenizer.getCurrentToken())) {
            eatCurrentToken()

            // (',' varName)*
            while(isSymbolEqual(_tokenizer.getCurrentToken(), ",")){
              eatCurrentToken()// eat ','

              // varName
              if (isIdentifier(_tokenizer.getCurrentToken())){
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

  /*
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

        eatCurrentToken()

        // ("void" | type)
        if (isKeywordEqual(_tokenizer.getCurrentToken(), "void") || isType(_tokenizer.getCurrentToken())){
          eatCurrentToken()

          // subroutineName
          if (isIdentifier(_tokenizer.getCurrentToken())){
            eatCurrentToken()

            // '('
            if (isSymbolEqual(_tokenizer.getCurrentToken(), "(")){
              eatCurrentToken()

              // parameterList
              if (compileParameterList()){

                // ')'
                if (isSymbolEqual(_tokenizer.getCurrentToken(), ")")){
                  eatCurrentToken()

                  // subroutineBody
                  if (compileSubroutineBody()){
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

    return false;
  }

  /*
   * Compiles a (possibly empty) parameter list.
   * Doesn't handle the enclosing "()".
   * ((type varName) (',' type varName)*)?
   */
  def compileParameterList():Boolean ={
    val tag = "parameterList"
    writeOpeningTag(tag)

    // type
    if (isType(_tokenizer.getCurrentToken())){
      eatCurrentToken()

      // varName
      if (isIdentifier(_tokenizer.getCurrentToken())){
        eatCurrentToken()

        // (',' type varName)*
        while (isSymbolEqual(_tokenizer.getCurrentToken(), ",")){
          eatCurrentToken() // ','

          // type
          if (isType(_tokenizer.getCurrentToken())){
            eatCurrentToken()

            // varName
            if (isIdentifier(_tokenizer.getCurrentToken())){
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

    return true
  }

  /*
   * Compiles a subroutine's body
   * '{' varDec* statements '}'
   */
  def compileSubroutineBody():Boolean ={
    if (_tokenizer.hasMoreTokens()){

      // '{'
      if (isSymbolEqual(_tokenizer.getCurrentToken(), "{")){
        val tag = "subroutineBody"
        writeOpeningTag(tag)

        eatCurrentToken()

        // varDec*
        while (compileVarDec()){}

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

  /*
   * Compiles a var declaration.
   * "var" type varName (',' varName)* ';'
   */
  def compileVarDec():Boolean ={
    if (_tokenizer.hasMoreTokens()){

      // "var"
      if (_tokenizer.tokenType() == TokenType.KEYWORD &&
        _tokenizer.keyword() == "var"){

        val tag = "varDec"
        writeOpeningTag(tag)

        eatCurrentToken()

        // type
        if (isType(_tokenizer.getCurrentToken())){
          eatCurrentToken()

          // varName
          if (isIdentifier(_tokenizer.getCurrentToken())){
            eatCurrentToken()

            // (',' varName)*
            while (isSymbolEqual(_tokenizer.getCurrentToken(), ",")){
              eatCurrentToken() // eat ','

              // varName
              if (isIdentifier(_tokenizer.getCurrentToken())){
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

  /*
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
    if (_tokenizer.hasMoreTokens()){

      // "let"
      if (isKeywordEqual(_tokenizer.getCurrentToken(), "let")){

        val tag = "letStatement"
        writeOpeningTag(tag)

        eatCurrentToken()

        // varName
        if (isIdentifier(_tokenizer.getCurrentToken())){
          eatCurrentToken()

          //('[' expression ']')?
          if (isSymbolEqual(_tokenizer.getCurrentToken(), "[")){
            eatCurrentToken() // eat '['

            // expression
            if (compileExpression()){

              // ']'
              if (isSymbolEqual(_tokenizer.getCurrentToken() ,"]")){
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

              // ','
              if (isSymbolEqual(_tokenizer.getCurrentToken(), ";")){
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

  /*
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
                      eatCurrentToken()

                      // '{'
                      if (isSymbolEqual(_tokenizer.getCurrentToken(), "{")){
                        eatCurrentToken()

                        // statements
                        if (compileStatements()){

                          // '}'
                          if (isSymbolEqual(_tokenizer.getCurrentToken(), "}")){
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

  /*
   * Compiles a while statement.
   * "while" '(' expression ')' '{' statements '}'
   */
  def compileWhile():Boolean ={
    if (_tokenizer.hasMoreTokens()){

      // "while"
      if (isKeywordEqual(_tokenizer.getCurrentToken(), "while")){
        val tag = "whileStatement"
        writeOpeningTag(tag)

        eatCurrentToken()

        if (isSymbolEqual(_tokenizer.getCurrentToken(), "(")){
          eatCurrentToken()

          // expression
          if (compileExpression()){

            // ')'
            if (isSymbolEqual(_tokenizer.getCurrentToken(), ")")){
              eatCurrentToken()

              // '{'
              if (isSymbolEqual(_tokenizer.getCurrentToken(), "{")){
                eatCurrentToken()

                // statements
                if (compileStatements()){

                  // '}'
                  if (isSymbolEqual(_tokenizer.getCurrentToken(), "}")){
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

  /*
   * Compiles a do statement
   * "do" subroutineCall ';'
   */
  def compileDo():Boolean ={
    if (_tokenizer.hasMoreTokens()){

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

            writeClosingTag(tag)

            return true
          }
        }

        writeClosingTag(tag)
      }
    }

    return false
  }

  /*
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
        compileExpression()

        // ';'
        if (isSymbolEqual(_tokenizer.getCurrentToken(), ";")){
          eatCurrentToken()

          writeClosingTag(tag)

          return true
        }

        writeClosingTag(tag)
      }
    }

    return false
  }

  /*
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
        while (isOp(_tokenizer.getCurrentToken())){
          eatCurrentToken()

          // term
          if (!compileTerm()){
            writeClosingTag(tag)

            // TODO: throw? expected term
            return false
          }
        }

        writeClosingTag(tag)

        return true
      }



      writeClosingTag(tag)

      return false
    }

    return false
  }

  /*
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

    /*    integerConstant | stringConstant | keywordConstant    */
    if (isIntConst(currentToken) || isStringConst(currentToken) || isKeywordConst(currentToken)){
      writeOpeningTag(tag)

      eatCurrentToken()

      writeClosingTag(tag)

      return true
    }
    /*    varName'['expression']' <=> identifier'[' etc...    */
    else if (isIdentifier(currentToken) && isSymbolEqual(nextToken, "[")){
      writeOpeningTag(tag)

      eatCurrentToken() // eat varName
      eatCurrentToken() // eat '['

      // expression
      if (compileExpression()){

        // ']'
        if (isSymbolEqual(_tokenizer.getCurrentToken(), "]")){
          eatCurrentToken()

          writeClosingTag(tag)

          return true
        }
      }

      writeClosingTag(tag)

      // TODO: throw? expected expression']'
      return false
    }
    /*  subroutineCall => (className|varName)'.'subroutineName'('expressionList')'  */
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

      eatCurrentToken()

      writeClosingTag(tag)

      return true
    }
    // '('expression')'
    else if (isSymbolEqual(currentToken, "(")){
      writeOpeningTag(tag) // eat '('

      eatCurrentToken()

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

      eatCurrentToken()

      // term
      if (compileTerm()){
        writeClosingTag(tag)

        return true
      }

      writeClosingTag(tag)

      return false
    }

    return false
  }

  /*
   * Compiles a (possibly empty) comma-separated list of expressions.
   * (expression (',' expression)* )?
   */
  def compileExpressionList():Boolean ={
    val tag = "expressionList"
    writeOpeningTag(tag)

    // expression
    if (compileExpression()){

      // (',' expression)*
      while (isSymbolEqual(_tokenizer.getCurrentToken(), ",")){
        eatCurrentToken() // eat ','

        if (!compileExpression()){
          writeClosingTag(tag)

          // TODO: throw? expected expression after ','
          return false
        }
      }
    }

    writeClosingTag(tag)

    return true
  }

  /*
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

  /*
   * Compiles a subroutineCall.
   * subroutineName '(' expressionList ')' |
   * (className | varName)'.' subroutineName '(' expressionList ')'
   *                              <=>
   * identifier ('('expressionList')' | '.'subroutineName'('expressionList')')
   */
  private def compileSubroutineCall():Boolean ={

    // subroutineName | className | varName <=> identifier
    if (isIdentifier(_tokenizer.getCurrentToken())){
      eatCurrentToken()

      // '(' => subroutineName '(' expressionList')'
      if (isSymbolEqual(_tokenizer.getCurrentToken(), "(")){
        eatCurrentToken()

        // expressionList
        if (compileExpressionList()){

          // ')'
          if (isSymbolEqual(_tokenizer.getCurrentToken(), ")")){
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
          eatCurrentToken()

          // '('
          if (isSymbolEqual(_tokenizer.getCurrentToken(), "(")){
            eatCurrentToken()

            // expressionList
            if (compileExpressionList()){

              // ')'
              if (isSymbolEqual(_tokenizer.getCurrentToken(), ")")){
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
    val writer = new FileWriter(_outputFileUrl, true)

    var i = 0;
    while (i < _tabsCount * 2){
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
    val writer = new FileWriter(_outputFileUrl, true)

    var i = 0
    while(i < _tabsCount * 2){
      writer.write(' ')

      i += 1
    }

    writer.write("<" + str + ">\n")
    writer.close()

    _tabsCount += 1
  }

  private def writeClosingTag(str:String): Unit ={
    _tabsCount -= 1

    val writer = new FileWriter(_outputFileUrl, true)

    var i = 0
    while(i < _tabsCount * 2){
      writer.write(' ')

      i += 1
    }

    writer.write("</" + str + ">\n")
    writer.close()

  }
}

