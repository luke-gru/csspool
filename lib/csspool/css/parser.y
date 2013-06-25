class CSSPool::CSS::Parser

token CHARSET_SYM IMPORT_SYM STRING SEMI IDENT S COMMA LBRACE RBRACE STAR HASH
token LSQUARE RSQUARE EQUAL INCLUDES DASHMATCH LPAREN RPAREN FUNCTION PSEUDOELEMENT GREATER PLUS
token SLASH NUMBER MINUS LENGTH PERCENTAGE EMS EXS ANGLE TIME FREQ URI
token IMPORTANT_SYM MEDIA_SYM
token ONLY NOT AND RESOLUTION

rule
  document
    : { @handler.start_document }
      stylesheet
      { @handler.end_document }
    ;
  stylesheet
    : charset stylesheet
    | import stylesheet
    | charset
    | import
    | body
    ;
  charset
    : CHARSET_SYM STRING SEMI { @handler.charset interpret_string(val[1]), {} }
    ;
  import
    : IMPORT_SYM import_location medium SEMI {
        @handler.import_style [val[2]].flatten, val[1]
      }
    | IMPORT_SYM import_location SEMI {
        @handler.import_style [], val[1]
      }
    ;
  import_location
    : import_location S
    | STRING { result = Terms::String.new interpret_string val.first }
    | URI { result = Terms::URI.new interpret_uri val.first }
    ;
  medium
    : medium COMMA IDENT {
        result = [val.first, Terms::Ident.new(interpret_identifier val.last)]
      }
    | IDENT {
        result = Terms::Ident.new interpret_identifier val.first
      }
    ;

  # start added

  media_query_list:
    media_query                           { result = [val[0]] }
  | media_query_list COMMA media_query    { result = val[0] << val[2] }
  | /* nothing */                         { result = nil }
  ;

  media_query:
    optional_only_or_not media_type optional_and_exprs   { result = MediaQuery.new(val[0], val[1], val[2]) }
  | media_expr optional_and_exprs                        { result = MediaQuery.new(nil, val[0], val[1]) }
  ;

  optional_only_or_not:
    ONLY S              { result = :only }
  | NOT S               { result = :not }
  | /* nothing */       { result = nil }
  ;

  media_type:
    IDENT               { result = MediaType.new(val[0]) }
  ;

  media_expr:
    LPAREN IDENT RPAREN                           { result = MediaType.new(val[1]) }
  | LPAREN IDENT ':' S expr RPAREN                { result = MediaFeature.new(val[1], val[4][0])}
  | LPAREN IDENT S ':' expr RPAREN                { result = MediaFeature.new(val[1], val[4][0])}
  | LPAREN IDENT ':' expr RPAREN                  { result = MediaFeature.new(val[1], val[3][0])}
  ;

  optional_and_exprs:
    optional_and_exprs S AND media_expr  { result = val[0].concat([val[3]]) }
  | /* nothing */                        { result = []}
  ;

  resolution:
    RESOLUTION {
      unit = val.first.gsub(/[\s\d.]/, '')
      number = numeric(val.first)
      result = Terms::Resolution.new(number, unit)
    }
  ;

# end added

  body
    : ruleset body
    | media body
    | ruleset
    | media
    ;
  media
    : start_media body RBRACE { @handler.end_media val.first }
    ;
  start_media
    : MEDIA_SYM media_query_list LBRACE {
        result = val[1]
        @handler.start_media result
      }
    | MEDIA_SYM LBRACE { result = [] }
    ;
  ruleset
    : start_selector declarations RBRACE {
        @handler.end_selector val.first
      }
    | start_selector RBRACE {
        @handler.end_selector val.first
      }
    ;
  start_selector
    : S start_selector { result = val.last }
    | selectors LBRACE {
        @handler.start_selector val.first
      }
    ;
  selectors
    : selector COMMA selectors
      {
        # FIXME: should always garantee array
        sel = Selector.new(val.first, {})
        result = [sel, val[2]].flatten
      }
    | selector
      {
        result = [Selector.new(val.first, {})]
      }
    ;
  selector
    : simple_selector combinator selector
      {
        val = val.flatten
        val[2].combinator = val.delete_at 1
        result = val
      }
    | simple_selector
    ;
  combinator
    : S       { result = :s }
    | GREATER { result = :> }
    | PLUS    { result = :+ }
    ;
  simple_selector
    : element_name hcap {
        selector = val.first
        selector.additional_selectors = val.last
        result = [selector]
      }
    | element_name { result = val }
    | hcap
      {
        ss = Selectors::Simple.new nil, nil
        ss.additional_selectors = val.flatten
        result = [ss]
      }
    ;
  element_name
    : IDENT { result = Selectors::Type.new interpret_identifier val.first }
    | STAR  { result = Selectors::Universal.new val.first }
    ;
  hcap
    : hash        { result = val }
    | class       { result = val }
    | attrib      { result = val }
    | pseudo      { result = val }
    | hash hcap   { result = val.flatten }
    | class hcap  { result = val.flatten }
    | attrib hcap { result = val.flatten }
    | pseudo hcap { result = val.flatten }
    ;
  hash
    : HASH {
        result = Selectors::Id.new interpret_identifier val.first.sub(/^#/, '')
      }
  class
    : '.' IDENT {
        result = Selectors::Class.new interpret_identifier val.last
      }
    ;
  attrib
    : LSQUARE IDENT EQUAL IDENT RSQUARE {
        result = Selectors::Attribute.new(
          interpret_identifier(val[1]),
          interpret_identifier(val[3]),
          Selectors::Attribute::EQUALS
        )
      }
    | LSQUARE IDENT EQUAL STRING RSQUARE {
        result = Selectors::Attribute.new(
          interpret_identifier(val[1]),
          interpret_string(val[3]),
          Selectors::Attribute::EQUALS
        )
      }
    | LSQUARE IDENT INCLUDES STRING RSQUARE {
        result = Selectors::Attribute.new(
          interpret_identifier(val[1]),
          interpret_string(val[3]),
          Selectors::Attribute::INCLUDES
        )
      }
    | LSQUARE IDENT INCLUDES IDENT RSQUARE {
        result = Selectors::Attribute.new(
          interpret_identifier(val[1]),
          interpret_identifier(val[3]),
          Selectors::Attribute::INCLUDES
        )
      }
    | LSQUARE IDENT DASHMATCH IDENT RSQUARE {
        result = Selectors::Attribute.new(
          interpret_identifier(val[1]),
          interpret_identifier(val[3]),
          Selectors::Attribute::DASHMATCH
        )
      }
    | LSQUARE IDENT DASHMATCH STRING RSQUARE {
        result = Selectors::Attribute.new(
          interpret_identifier(val[1]),
          interpret_string(val[3]),
          Selectors::Attribute::DASHMATCH
        )
      }
    | LSQUARE IDENT RSQUARE {
        result = Selectors::Attribute.new(
          interpret_identifier(val[1]),
          nil,
          Selectors::Attribute::SET
        )
      }
    ;
  pseudo
    : pseudo_scope IDENT {
        result = Selectors::PseudoClass.new(
          interpret_identifier(val[1])
        )
      }
    | pseudo_scope PSEUDOELEMENT {
        result = Selectors::PseudoElement.new(
          val[1]
        )
      }
    | pseudo_scope FUNCTION RPAREN {
        result = Selectors::PseudoClass.new(
          interpret_identifier(val[1].sub(/\($/, '')),
          ''
        )
      }
    | pseudo_scope FUNCTION IDENT RPAREN {
        result = Selectors::PseudoClass.new(
          interpret_identifier(val[1].sub(/\($/, '')),
          interpret_identifier(val[2])
        )
      }
    ;

    pseudo_scope:
      ':'   { result = val[0] }
    | '::'  { result = val[0] }
    ;

  declarations
    : declaration SEMI declarations
    | declaration SEMI
    | declaration
    ;
  declaration
    : property ':' expr prio
      { @handler.property val.first, val[2], val[3] }
    | property ':' S expr prio
      { @handler.property val.first, val[3], val[4] }
    | property S ':' expr prio
      { @handler.property val.first, val[3], val[4] }
    | property S ':' S expr prio
      { @handler.property val.first, val[4], val[5] }
    ;
  prio
    : IMPORTANT_SYM { result = true }
    |               { result = false }
    ;
  property
    : IDENT { result = interpret_identifier val[0] }
    | STAR IDENT { result = interpret_identifier val.join }
    ;
  operator
    : COMMA
    | SLASH
    | EQUAL
    ;
  expr
    : term operator expr {
        result = [val.first, val.last].flatten
        val.last.first.operator = val[1]
      }
    | term expr { result = val.flatten }
    | term      { result = val }
    ;
  term
    : ident
    | numeric
    | string
    | uri
    | hexcolor
    | function
    | resolution
    ;
  function
    : function S { result = val.first }
    | FUNCTION expr RPAREN {
        name = interpret_identifier val.first.sub(/\($/, '')
        if name == 'rgb'
          result = Terms::Rgb.new(*val[1])
        else
          result = Terms::Function.new name, val[1]
        end
      }
    ;
  hexcolor
    : hexcolor S { result = val.first }
    | HASH { result = Terms::Hash.new val.first.sub(/^#/, '') }
    ;
  uri
    : uri S { result = val.first }
    | URI { result = Terms::URI.new interpret_uri val.first }
  string
    : string S { result = val.first }
    | STRING { result = Terms::String.new interpret_string val.first }
    ;
  numeric
    : unary_operator numeric {
        result = val[1]
        val[1].unary_operator = val.first
      }
    | NUMBER {
        result = Terms::Number.new numeric val.first
      }
    | PERCENTAGE {
        result = Terms::Number.new numeric(val.first), nil, '%'
      }
    | LENGTH {
        unit    = val.first.gsub(/[\s\d.]/, '')
        result = Terms::Number.new numeric(val.first), nil, unit
      }
    | EMS {
        result = Terms::Number.new numeric(val.first), nil, 'em'
      }
    | EXS {
        result = Terms::Number.new numeric(val.first), nil, 'ex'
      }
    | ANGLE {
        unit    = val.first.gsub(/[\s\d.]/, '')
        result = Terms::Number.new numeric(val.first), nil, unit
      }
    | TIME {
        unit    = val.first.gsub(/[\s\d.]/, '')
        result = Terms::Number.new numeric(val.first), nil, unit
      }
    | FREQ {
        unit    = val.first.gsub(/[\s\d.]/, '')
        result = Terms::Number.new numeric(val.first), nil, unit
      }
    ;
  unary_operator
    : MINUS { result = :minus }
    | PLUS  { result = :plus }
    ;
  ident
    : ident S { result = val.first }
    | IDENT { result = Terms::Ident.new interpret_identifier val.first }
    ;

---- inner

def numeric thing
  thing = thing.gsub(/[^\d.]/, '')
  Integer(thing) rescue Float(thing)
end

def interpret_identifier s
  interpret_escapes s
end

def interpret_uri s
  interpret_escapes s.match(/^url\((.*)\)$/mu)[1].strip.match(/^(['"]?)((?:\\.|.)*)\1$/mu)[2]
end

def interpret_string s
  interpret_escapes s.match(/^(['"])((?:\\.|.)*)\1$/mu)[2]
end

def interpret_escapes s
  token_exp = /\\([0-9a-fA-F]{1,6}(?:\r\n|\s)?)|\\(.)|(.)/mu
  characters = s.scan(token_exp).map do |u_escape, i_escape, ident|
    if u_escape
      code = u_escape.chomp.to_i 16
      code = 0xFFFD if code > 0x10FFFF
      [code].pack('U')
    elsif i_escape
      if i_escape == "\n"
        ''
      else
        i_escape
      end
    else
      ident
    end
  end.join ''
end
