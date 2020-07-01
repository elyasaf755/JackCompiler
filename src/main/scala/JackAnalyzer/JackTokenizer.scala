package Modules.JackAnalyzerClasses

import Modules.JackAnalyzerClasses.TokenType.TokenType
import Tools.Util
import scala.collection.mutable.ListBuffer
import scala.io.Source

class JackTokenizer {

  private var _currentToken:Token = _
  private var _fileContentsString:String = ""
  private var _tokensList = new ListBuffer[Token]
  var _index = -1

  private val _keywords:List[String] = List(
    "class", "constructor", "function", "method", "field", "static",
    "var", "int", "char", "boolean", "void", "true", "false",
    "null", "this", "let", "do", "if", "else", "while", "return"
  )
  private val _symbols:List[Char] = List(
    '{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*',
    '/', '&', '|', '<', '>', '=', '~'
  )
  private val _symbolsMap:Map[Char, String] = Map(
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

  def this(inputFileUrl:String){
    this()

    Source.fromFile(inputFileUrl).getLines.foreach(line => {
      _fileContentsString += line + '\n'
    })


    tokenize()
  }

  /*
   * Returns true if there are more tokens in the input.
   */
  def hasMoreTokens():Boolean ={
    return _index < _tokensList.length && _tokensList.length > 0
  }

  /*
   * Gets the next token from the input,
   * and makes it the current token.
   * This method should be called only if hasMoreTokens is true.
   * Initially there is no current token.
   */
  def advance():Unit = {
    _index += 1

    if (_index < _tokensList.length){
      _currentToken = _tokensList(_index)
    }
  }

  /*
   * Returns the type of the current token.
   */
  def tokenType():TokenType ={
    return _currentToken.tokenType;
  }

  /*
   * Returns the keyword which is the current token.
   * Should be called only if tokenType is KEYWORD.
   */
  def keyword():String ={
    if (_currentToken.tokenType == TokenType.KEYWORD){
      return _currentToken.tokenValue;
    }

    return null.asInstanceOf[String]
  }

  /*
   * Return the character which is the current token.
   * Should be called only if tokenType is SYMBOL.
   */
  def symbol():Char ={
    if (_currentToken.tokenType == TokenType.SYMBOL){
      val charArray = _currentToken.tokenValue.toCharArray()

      if (charArray.length == 1){
        return _currentToken.tokenValue.toCharArray()(0);
      }
    }

    return null.asInstanceOf[Char]
  }

  /*
   * Returns the identifier which is the current token.
   * Should be called only if tokenType is IDENTIFIER.
   */
  def identifier():String ={
    if (_currentToken.tokenType == TokenType.IDENTIFIER){
      return _currentToken.tokenValue;
    }

    return null.asInstanceOf[String]
  }

  /*
   * Returns the integer value of the current token.
   * Should be called only if tokenType is INT_COST.
   */
  def intVal():Int ={
    if (_currentToken.tokenType == TokenType.INT_CONST){
      if (Util.isInt(_currentToken.tokenValue)){
        return _currentToken.tokenValue.toInt;
      }
    }

    return null.asInstanceOf[Int]
  }

  /*
   * Returns the string value of the current token.
   * Should be called only if tokenType is STRING_COST.
   */
  def stringVal():String ={
    if (_currentToken.tokenType == TokenType.STRING_CONST){
      return _currentToken.tokenValue;
    }

    return null.asInstanceOf[String]
  }

  /*
   * Returns the current token
   */
  def getCurrentToken():Token ={
    return _currentToken;
  }

  /*
   * Returns the value of the current token
   */
  def getCurrentTokenValue():String ={
    return _currentToken.tokenValue
  }

  /*
   * Returns the tokens list
   */
  def getTokens():ListBuffer[Token] ={
    return _tokensList
  }

  /*
   * Creates a tokens list out of the input .jack file
   */
  def tokenize():Unit ={
    var i = 0
    val str = _fileContentsString
    val length = str.length
    var currentTokenValue = "";


    while (i < length){
      var currentChar = str(i)

      if (_tokensList.length > 122){
        var asd = 5+4
      }

      // boundary reached. handles comments
      if (currentChar == '/'){
        val commentLength = getCommentLength(i)
        if (commentLength != -1){
          if (isKeyword(currentTokenValue)){
            _tokensList.addOne(new Token(TokenType.KEYWORD, currentTokenValue))
          }
          else if (Util.isInt(currentTokenValue)){
            _tokensList.addOne(new Token(TokenType.INT_CONST, currentTokenValue))
          }
          // MUST BE THE LAST CONDITION OF THIS IF-ELSE STATEMENT
          else if (!isWhiteSpace(currentTokenValue)){
            // TODO: IDENTIFIER can't start with a digit
            _tokensList.addOne(new Token(TokenType.IDENTIFIER, currentTokenValue))
          }

          // we always increment +1 at the end of the loop,
          // so we need to subtract 1
          i += commentLength - 1

          // reset token value to find a new one
          currentTokenValue = ""
        }
        // '/' is a symbol
        else if (isSymbol(currentChar)){
          _tokensList.addOne(new Token(TokenType.SYMBOL, _symbolsMap(currentChar)))

          // reset token value to find a new one
          currentTokenValue = ""
        }
      }
      // handles constant strings
      else if (currentChar == '\"') {
        currentTokenValue = getQuotedString(i)
        if (currentTokenValue != null.asInstanceOf[String]) {
          _tokensList.addOne(new Token(TokenType.STRING_CONST, currentTokenValue))

          // string length + 2 quotes.
          // we always increment +1 at the end of the loop,
          // so we need to subtract 1
          i += currentTokenValue.length + 2 - 1

          currentTokenValue = ""
        }
        else {
          // TODO: Handle a potential error?
        }
      }
      // boundary reached. handles white spaces
      else if (isWhiteSpace(currentChar)){
        if (isKeyword(currentTokenValue)){
          _tokensList.addOne(new Token(TokenType.KEYWORD, currentTokenValue))
        }
        else if (Util.isInt(currentTokenValue)){
          _tokensList.addOne(new Token(TokenType.INT_CONST, currentTokenValue))
        }
        // MUST BE THE LAST CONDITION OF THIS IF-ELSE STATEMENT
        else if (!isWhiteSpace(currentTokenValue)){
          // TODO: IDENTIFIER can't start with a digit
          _tokensList.addOne(new Token(TokenType.IDENTIFIER, currentTokenValue))
        }

        // reset token value to find a new one
        currentTokenValue = ""
      }
      // boundary reached. handles symbols
      else if (isSymbol(currentChar)){
        if (isKeyword(currentTokenValue)){
          _tokensList.addOne(new Token(TokenType.KEYWORD, currentTokenValue))
        }
        else if (Util.isInt(currentTokenValue)){
          _tokensList.addOne(new Token(TokenType.INT_CONST, currentTokenValue))
        }
        // MUST BE THE LAST CONDITION OF THIS IF-ELSE STATEMENT
        else if (!isWhiteSpace(currentTokenValue)){
          // TODO: IDENTIFIER can't start with a digit
          _tokensList.addOne(new Token(TokenType.IDENTIFIER, currentTokenValue))
        }

        _tokensList.addOne(new Token(TokenType.SYMBOL, _symbolsMap(currentChar)))

        // reset token value to find a new one
        currentTokenValue = ""
      }
      // boundary NOT reached yet
      else{
        currentTokenValue += currentChar
      }

      i += 1;
    }
  }

  /*
   * Returns the next token.
   * Useful when one look-ahead is needed
   */
  def getNextToken():Token ={
    return _tokensList(_index + 1)
  }

  /*
   * Returns the next n'th token.
   * Useful when n-look-ahead is needed (not needed in Jack Programming Language).
   */
  def peek(num:Int):Token ={
    val index = _index + num

    if (index < _tokensList.length && index >= 0){
      return _tokensList(index)
    }

    return null.asInstanceOf[Token]
  }

  /*
   * Returns the length of the comment starting at index.
   * If it's not a comment, returns -1.
   */
  private def getCommentLength(index:Int):Int ={
    val str = _fileContentsString
    val length = str.length

    if (str(index) == '/'){
      if (index + 1 < length){

        // If comment starts with "//"
        if (str(index + 1) == '/'){
          var i = index + 2;
          while (i < length && str(i) != '\n'){
            i = i + 1;
          }

          return i - index;
        }

        // If comment starts with "/*"
        if (str(index + 1) == '*'){
          var i = index + 2;
          var flag = true;
          while (flag && i < length){
            if (str(i) == '*'){
              i = i + 1;
              if (i < length && str(i) == '/'){
                i = i + 1;
                flag = false;
              }
            }
            else{
              i = i + 1
            }
          }

          return i - index;
        }

      }
    }

    return -1
  }

  /*
   * Returns the quoted string starting at index
   * If it's not starting or ending with a quote, returns null.
   */
  private def getQuotedString(index:Int):String ={
    val str = _fileContentsString
    val length = str.length
    var result = ""
    var i = index

    // if starts with a quote
    if (str(i) == '\"'){
      i = i + 1
      while(i < length && str(i) != '\"'){
        result = result + str(i)

        i = i +1
      }
    }

    // TODO: const string is a sequence of chars not including double quotes or new lines.

    // if ends with a quote
    if (i < length && str(i) == '\"'){
      return result
    }

    return null.asInstanceOf[String]
  }

  /*
   * Returns true if a char is a white-space char
   */
  private def isWhiteSpace(char:Char):Boolean ={
    return char.toString.trim == ""
  }

  /*
   * Returns true if a string is a white-space string
   */
  private def isWhiteSpace(str:String):Boolean ={
    return str.trim == ""
  }

  /*
   * Returns true if a char is a symbol
   */
  private def isSymbol(char:Char):Boolean ={
    return _symbols.contains(char)
  }

  /*
   * Returns true if a string is a constant keyword
   */
  private def isKeyword(str:String):Boolean ={
    return _keywords.contains(str)
  }
}

class Token {
  var tokenType:TokenType = _
  var tokenValue:String = _

  def this(tType:TokenType, tValue:String){
    this()

    tokenType = tType
    tokenValue = tValue
  }

  def toXML():String ={
    return "<" + tokenType.toString() + "> " + tokenValue + " </" + tokenType.toString() + ">"
  }
}

object TokenType extends Enumeration {
  type TokenType = Value

  // Assigning values
  val KEYWORD = Value("keyword")
  val SYMBOL = Value("symbol")
  val IDENTIFIER = Value("identifier")
  val INT_CONST = Value("integerConstant")
  val STRING_CONST = Value("stringConstant")
}