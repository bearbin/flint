//
//  TypeChecker.swift
//  flintc
//
//  Created by Franklin Schrans on 1/11/18.
//

import AST

public struct TypeChecker: ASTPass {
  public init() {}

  func type(of expression: Expression, functionDeclarationContext: FunctionDeclarationContext, environment: Environment) -> Type.RawType {
    switch expression {
    case .binaryExpression(let binaryExpression):
      return type(of: binaryExpression.rhs, functionDeclarationContext: functionDeclarationContext, environment: environment)

    case .bracketedExpression(let expression):
      return type(of: expression, functionDeclarationContext: functionDeclarationContext, environment: environment)

    case .functionCall(let functionCall):
      let contractContext = functionDeclarationContext.contractContext
      return environment.type(of: functionCall, contractIdentifier: contractContext.contractIdentifier, callerCapabilities: contractContext.callerCapabilities) ?? .errorType

    case .identifier(let identifier):
      if !identifier.isPropertyAccess, let localVariable = functionDeclarationContext.declaration.matchingLocalVariable(identifier) {
        return localVariable.type.rawType
      }
      return environment.type(of: identifier, contractIdentifier: functionDeclarationContext.contractContext.contractIdentifier)!

    case .literal(let token):
      guard case .literal(let literal) = token.kind else { fatalError() }
      switch literal {
      case .boolean(_): return .builtInType(.bool)
      case .decimal(.integer(_)): return .builtInType(.int)
      default: fatalError()
      }
    case .self(_): return .userDefinedType(functionDeclarationContext.contractContext.contractIdentifier.name)
    case .variableDeclaration(let variableDeclaration):
      return variableDeclaration.type.rawType
    case .subscriptExpression(let subscriptExpression):
      var type = environment.type(of: subscriptExpression.baseIdentifier, contractIdentifier: functionDeclarationContext.contractContext.contractIdentifier)!

      for _ in subscriptExpression.indexExpressions {
        switch type {
          case .arrayType(let elementType): type = elementType
          case .fixedSizeArrayType(let elementType, _): type = elementType
          case .dictionaryType(_, let valueType): type = valueType
          default: break
        }
      }

      return type
    }
  }
  
  public func process(topLevelModule: TopLevelModule, passContext: ASTPassContext) -> ASTPassResult<TopLevelModule> {
    return ASTPassResult(element: topLevelModule, diagnostics: [], passContext: passContext)
  }

  public func process(topLevelDeclaration: TopLevelDeclaration, passContext: ASTPassContext) -> ASTPassResult<TopLevelDeclaration> {
    return ASTPassResult(element: topLevelDeclaration, diagnostics: [], passContext: passContext)
  }

  public func process(contractDeclaration: ContractDeclaration, passContext: ASTPassContext) -> ASTPassResult<ContractDeclaration> {
    return ASTPassResult(element: contractDeclaration, diagnostics: [], passContext: passContext)
  }

  public func process(contractBehaviorDeclaration: ContractBehaviorDeclaration, passContext: ASTPassContext) -> ASTPassResult<ContractBehaviorDeclaration> {
    return ASTPassResult(element: contractBehaviorDeclaration, diagnostics: [], passContext: passContext)
  }

  public func process(variableDeclaration: VariableDeclaration, passContext: ASTPassContext) -> ASTPassResult<VariableDeclaration> {
    return ASTPassResult(element: variableDeclaration, diagnostics: [], passContext: passContext)
  }

  public func process(functionDeclaration: FunctionDeclaration, passContext: ASTPassContext) -> ASTPassResult<FunctionDeclaration> {
    return ASTPassResult(element: functionDeclaration, diagnostics: [], passContext: passContext)
  }

  public func process(attribute: Attribute, passContext: ASTPassContext) -> ASTPassResult<Attribute> {
    return ASTPassResult(element: attribute, diagnostics: [], passContext: passContext)
  }

  public func process(parameter: Parameter, passContext: ASTPassContext) -> ASTPassResult<Parameter> {
    return ASTPassResult(element: parameter, diagnostics: [], passContext: passContext)
  }

  public func process(typeAnnotation: TypeAnnotation, passContext: ASTPassContext) -> ASTPassResult<TypeAnnotation> {
    return ASTPassResult(element: typeAnnotation, diagnostics: [], passContext: passContext)
  }

  public func process(identifier: Identifier, passContext: ASTPassContext) -> ASTPassResult<Identifier> {
    return ASTPassResult(element: identifier, diagnostics: [], passContext: passContext)
  }

  public func process(type: Type, passContext: ASTPassContext) -> ASTPassResult<Type> {
    return ASTPassResult(element: type, diagnostics: [], passContext: passContext)
  }

  public func process(callerCapability: CallerCapability, passContext: ASTPassContext) -> ASTPassResult<CallerCapability> {
    return ASTPassResult(element: callerCapability, diagnostics: [], passContext: passContext)
  }

  public func process(expression: Expression, passContext: ASTPassContext) -> ASTPassResult<Expression> {
    return ASTPassResult(element: expression, diagnostics: [], passContext: passContext)
  }

  public func process(statement: Statement, passContext: ASTPassContext) -> ASTPassResult<Statement> {
    return ASTPassResult(element: statement, diagnostics: [], passContext: passContext)
  }

  public func process(binaryExpression: BinaryExpression, passContext: ASTPassContext) -> ASTPassResult<BinaryExpression> {
    var diagnostics = [Diagnostic]()

    let environment = passContext.environment!
    let functionDeclarationContext = passContext.functionDeclarationContext!

    if case .punctuation(.equal) = binaryExpression.op.kind {
      let lhsType = type(of: binaryExpression.lhs, functionDeclarationContext: functionDeclarationContext, environment: environment)
      let rhsType = type(of: binaryExpression.rhs, functionDeclarationContext: functionDeclarationContext, environment: environment)

      if lhsType != rhsType, ![lhsType, rhsType].contains(.errorType) {
        diagnostics.append(.incompatibleAssignment(lhsType: lhsType, rhsType: rhsType, expression: .binaryExpression(binaryExpression)))
      }
    }

    return ASTPassResult(element: binaryExpression, diagnostics: diagnostics, passContext: passContext)
  }

  public func process(functionCall: FunctionCall, passContext: ASTPassContext) -> ASTPassResult<FunctionCall> {
    var diagnostics = [Diagnostic]()
    let environment = passContext.environment!
    let functionDeclarationContext = passContext.functionDeclarationContext!
    let contractIdentifier = functionDeclarationContext.contractContext.contractIdentifier

    if let eventCall = environment.matchEventCall(functionCall, contractIdentifier: contractIdentifier) {
      let expectedTypes = eventCall.type.genericArguments.map { $0.rawType }

      for (i, argument) in functionCall.arguments.enumerated() {
        let argumentType = type(of: argument, functionDeclarationContext: functionDeclarationContext, environment: environment)
        let expectedType = expectedTypes[i]
        if argumentType != expectedType {
          diagnostics.append(.incompatibleArgumentType(actualType: argumentType, expectedType: expectedType, expression: argument))
        }
      }
    }

    return ASTPassResult(element: functionCall, diagnostics: diagnostics, passContext: passContext)
  }

  public func process(subscriptExpression: SubscriptExpression, passContext: ASTPassContext) -> ASTPassResult<SubscriptExpression> {
    return ASTPassResult(element: subscriptExpression, diagnostics: [], passContext: passContext)
  }

  public func process(returnStatement: ReturnStatement, passContext: ASTPassContext) -> ASTPassResult<ReturnStatement> {
    var diagnostics = [Diagnostic]()
    let functionDeclarationContext = passContext.functionDeclarationContext!
    let environment = passContext.environment!

    if let expression = returnStatement.expression {
      let actualType = type(of: expression, functionDeclarationContext: functionDeclarationContext, environment: environment)
      let expectedType = functionDeclarationContext.declaration.rawType

      if actualType != expectedType {
        diagnostics.append(.incompatibleReturnType(actualType: actualType, expectedType: expectedType, expression: expression))
      }
    }

    return ASTPassResult(element: returnStatement, diagnostics: diagnostics, passContext: passContext)
  }

  public func process(ifStatement: IfStatement, passContext: ASTPassContext) -> ASTPassResult<IfStatement> {
    return ASTPassResult(element: ifStatement, diagnostics: [], passContext: passContext)
  }

  public func postProcess(topLevelModule: TopLevelModule, passContext: ASTPassContext) -> ASTPassResult<TopLevelModule> {
    return ASTPassResult(element: topLevelModule, diagnostics: [], passContext: passContext)
  }

  public func postProcess(topLevelDeclaration: TopLevelDeclaration, passContext: ASTPassContext) -> ASTPassResult<TopLevelDeclaration> {
    return ASTPassResult(element: topLevelDeclaration, diagnostics: [], passContext: passContext)
  }

  public func postProcess(contractDeclaration: ContractDeclaration, passContext: ASTPassContext) -> ASTPassResult<ContractDeclaration> {
    return ASTPassResult(element: contractDeclaration, diagnostics: [], passContext: passContext)
  }

  public func postProcess(contractBehaviorDeclaration: ContractBehaviorDeclaration, passContext: ASTPassContext) -> ASTPassResult<ContractBehaviorDeclaration> {
    return ASTPassResult(element: contractBehaviorDeclaration, diagnostics: [], passContext: passContext)
  }

  public func postProcess(variableDeclaration: VariableDeclaration, passContext: ASTPassContext) -> ASTPassResult<VariableDeclaration> {
    return ASTPassResult(element: variableDeclaration, diagnostics: [], passContext: passContext)
  }

  public func postProcess(functionDeclaration: FunctionDeclaration, passContext: ASTPassContext) -> ASTPassResult<FunctionDeclaration> {
    return ASTPassResult(element: functionDeclaration, diagnostics: [], passContext: passContext)
  }

  public func postProcess(attribute: Attribute, passContext: ASTPassContext) -> ASTPassResult<Attribute> {
    return ASTPassResult(element: attribute, diagnostics: [], passContext: passContext)
  }

  public func postProcess(parameter: Parameter, passContext: ASTPassContext) -> ASTPassResult<Parameter> {
    return ASTPassResult(element: parameter, diagnostics: [], passContext: passContext)
  }

  public func postProcess(typeAnnotation: TypeAnnotation, passContext: ASTPassContext) -> ASTPassResult<TypeAnnotation> {
    return ASTPassResult(element: typeAnnotation, diagnostics: [], passContext: passContext)
  }

  public func postProcess(identifier: Identifier, passContext: ASTPassContext) -> ASTPassResult<Identifier> {
    return ASTPassResult(element: identifier, diagnostics: [], passContext: passContext)
  }

  public func postProcess(type: Type, passContext: ASTPassContext) -> ASTPassResult<Type> {
    return ASTPassResult(element: type, diagnostics: [], passContext: passContext)
  }

  public func postProcess(callerCapability: CallerCapability, passContext: ASTPassContext) -> ASTPassResult<CallerCapability> {
    return ASTPassResult(element: callerCapability, diagnostics: [], passContext: passContext)
  }

  public func postProcess(expression: Expression, passContext: ASTPassContext) -> ASTPassResult<Expression> {
    return ASTPassResult(element: expression, diagnostics: [], passContext: passContext)
  }

  public func postProcess(statement: Statement, passContext: ASTPassContext) -> ASTPassResult<Statement> {
    return ASTPassResult(element: statement, diagnostics: [], passContext: passContext)
  }

  public func postProcess(binaryExpression: BinaryExpression, passContext: ASTPassContext) -> ASTPassResult<BinaryExpression> {
    return ASTPassResult(element: binaryExpression, diagnostics: [], passContext: passContext)
  }

  public func postProcess(functionCall: FunctionCall, passContext: ASTPassContext) -> ASTPassResult<FunctionCall> {
    return ASTPassResult(element: functionCall, diagnostics: [], passContext: passContext)
  }

  public func postProcess(subscriptExpression: SubscriptExpression, passContext: ASTPassContext) -> ASTPassResult<SubscriptExpression> {
    return ASTPassResult(element: subscriptExpression, diagnostics: [], passContext: passContext)
  }

  public func postProcess(returnStatement: ReturnStatement, passContext: ASTPassContext) -> ASTPassResult<ReturnStatement> {
    return ASTPassResult(element: returnStatement, diagnostics: [], passContext: passContext)
  }

  public func postProcess(ifStatement: IfStatement, passContext: ASTPassContext) -> ASTPassResult<IfStatement> {
    return ASTPassResult(element: ifStatement, diagnostics: [], passContext: passContext)
  }
}
