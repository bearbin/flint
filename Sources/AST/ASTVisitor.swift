//
//  ASTVisitor.swift
//  AST
//
//  Created by Franklin Schrans on 1/16/18.
//

public struct ASTVisitor<Pass: ASTPass> {
  var pass: Pass

  public init(pass: Pass) {
    self.pass = pass
  }

  public func visit(_ topLevelModule: TopLevelModule, passContext: ASTPassContext) -> ASTPassResult<TopLevelModule> {
    var processResult = pass.process(topLevelModule: topLevelModule, passContext: passContext)

    processResult.element.declarations = processResult.element.declarations.map { declaration in
      processResult.combining(visit(declaration, passContext: processResult.passContext))
    }

    let postProcessResult = pass.postProcess(topLevelModule: processResult.element, passContext: processResult.passContext)
    return ASTPassResult(element: postProcessResult.element, diagnostics: processResult.diagnostics + postProcessResult.diagnostics, passContext: postProcessResult.passContext)
  }

  func visit(_ topLevelDeclaration: TopLevelDeclaration, passContext: ASTPassContext) -> ASTPassResult<TopLevelDeclaration> {
    var processResult = pass.process(topLevelDeclaration: topLevelDeclaration, passContext: passContext)
    
    switch processResult.element {
    case .contractBehaviorDeclaration(let contractBehaviorDeclaration):
      processResult.element = .contractBehaviorDeclaration(processResult.combining(visit(contractBehaviorDeclaration, passContext: processResult.passContext)))
    case .contractDeclaration(let contractDeclaration):
      processResult.element = .contractDeclaration(processResult.combining(visit(contractDeclaration, passContext: processResult.passContext)))
    }

    let postProcessResult = pass.postProcess(topLevelDeclaration: processResult.element, passContext: processResult.passContext)
    return ASTPassResult(element: postProcessResult.element, diagnostics: processResult.diagnostics + postProcessResult.diagnostics, passContext: postProcessResult.passContext)
  }

  func visit(_ contractDeclaration: ContractDeclaration, passContext: ASTPassContext) -> ASTPassResult<ContractDeclaration> {
    var processResult = pass.process(contractDeclaration: contractDeclaration, passContext: passContext)

    processResult.element.identifier = processResult.combining(visit(processResult.element.identifier, passContext: processResult.passContext))

    processResult.element.variableDeclarations = processResult.element.variableDeclarations.map { variableDeclaration in
      return processResult.combining(visit(variableDeclaration, passContext: processResult.passContext))
    }

    let postProcessResult = pass.postProcess(contractDeclaration: processResult.element, passContext: processResult.passContext)
    return ASTPassResult(element: postProcessResult.element, diagnostics: processResult.diagnostics + postProcessResult.diagnostics, passContext: postProcessResult.passContext)
  }

  func visit(_ contractBehaviorDeclaration: ContractBehaviorDeclaration, passContext: ASTPassContext) -> ASTPassResult<ContractBehaviorDeclaration> {
    let properties = passContext.environment!.properties(declaredIn: contractBehaviorDeclaration.contractIdentifier)
    let declarationContext = ContractBehaviorDeclarationContext(contractIdentifier: contractBehaviorDeclaration.contractIdentifier, contractProperties: properties, callerCapabilities: contractBehaviorDeclaration.callerCapabilities)
    let passContext = passContext.withUpdates { $0.contractBehaviorDeclarationContext = declarationContext }

    var processResult = pass.process(contractBehaviorDeclaration: contractBehaviorDeclaration, passContext: passContext)

    processResult.element.contractIdentifier = processResult.combining(visit(processResult.element.contractIdentifier, passContext: processResult.passContext))

    if let capabilityBinding = processResult.element.capabilityBinding {
      processResult.element.capabilityBinding = processResult.combining(visit(capabilityBinding, passContext: processResult.passContext))
    }

    processResult.element.callerCapabilities = processResult.element.callerCapabilities.map { callerCapability in
      return processResult.combining(visit(callerCapability, passContext: processResult.passContext))
    }

    processResult.element.functionDeclarations = processResult.element.functionDeclarations.map { functionDeclaration in
      return processResult.combining(visit(functionDeclaration, passContext: processResult.passContext))
    }

    let postProcessResult = pass.postProcess(contractBehaviorDeclaration: processResult.element, passContext: processResult.passContext)
    return ASTPassResult(element: postProcessResult.element, diagnostics: processResult.diagnostics + postProcessResult.diagnostics, passContext: postProcessResult.passContext)
  }

  func visit(_ variableDeclaration: VariableDeclaration, passContext: ASTPassContext) -> ASTPassResult<VariableDeclaration> {
    var processResult = pass.process(variableDeclaration: variableDeclaration, passContext: passContext)

    processResult.element.identifier = processResult.combining(visit(processResult.element.identifier, passContext: processResult.passContext))
    processResult.element.type = processResult.combining(visit(processResult.element.type, passContext: processResult.passContext))

    let postProcessResult = pass.postProcess(variableDeclaration: processResult.element, passContext: processResult.passContext)
    return ASTPassResult(element: postProcessResult.element, diagnostics: processResult.diagnostics + postProcessResult.diagnostics, passContext: postProcessResult.passContext)
  }

  func visit(_ functionDeclaration: FunctionDeclaration, passContext: ASTPassContext) -> ASTPassResult<FunctionDeclaration> {
    let functionDeclarationContext = FunctionDeclarationContext(declaration: functionDeclaration, contractContext:  passContext.contractBehaviorDeclarationContext!)
    let passContext = passContext.withUpdates { $0.functionDeclarationContext = functionDeclarationContext }

    var processResult = pass.process(functionDeclaration: functionDeclaration, passContext: passContext)

    processResult.element.attributes = processResult.element.attributes.map { attribute in
      return processResult.combining(visit(attribute, passContext: processResult.passContext))
    }

    processResult.element.identifier = processResult.combining(visit(processResult.element.identifier, passContext: processResult.passContext))

    processResult.element.parameters = processResult.element.parameters.map { parameter in
      return processResult.combining(visit(parameter, passContext: processResult.passContext))
    }

    if let resultType = processResult.element.resultType {
      processResult.element.resultType = processResult.combining(visit(resultType, passContext: processResult.passContext))
    }

    processResult.element.body = processResult.element.body.map { statement in
      return processResult.combining(visit(statement, passContext: processResult.passContext))
    }

    let postProcessResult = pass.postProcess(functionDeclaration: processResult.element, passContext: processResult.passContext)
    return ASTPassResult(element: postProcessResult.element, diagnostics: processResult.diagnostics + postProcessResult.diagnostics, passContext: postProcessResult.passContext)
  }

  func visit(_ attribute: Attribute, passContext: ASTPassContext) -> ASTPassResult<Attribute> {
    let processResult = pass.process(attribute: attribute, passContext: passContext)

    let postProcessResult = pass.postProcess(attribute: processResult.element, passContext: processResult.passContext)
    return ASTPassResult(element: postProcessResult.element, diagnostics: processResult.diagnostics + postProcessResult.diagnostics, passContext: postProcessResult.passContext)
  }

  func visit(_ parameter: Parameter, passContext: ASTPassContext) -> ASTPassResult<Parameter> {
    var processResult = pass.process(parameter: parameter, passContext: passContext)
    processResult.element.type = processResult.combining(visit(processResult.element.type, passContext: processResult.passContext))

    let postProcessResult = pass.postProcess(parameter: processResult.element, passContext: processResult.passContext)
    return ASTPassResult(element: postProcessResult.element, diagnostics: processResult.diagnostics + postProcessResult.diagnostics, passContext: postProcessResult.passContext)
  }

  func visit(_ typeAnnotation: TypeAnnotation, passContext: ASTPassContext) -> ASTPassResult<TypeAnnotation> {
    var processResult = pass.process(typeAnnotation: typeAnnotation, passContext: passContext)
    processResult.element.type = processResult.combining(visit(processResult.element.type, passContext: processResult.passContext))

    let postProcessResult = pass.postProcess(typeAnnotation: processResult.element, passContext: processResult.passContext)
    return ASTPassResult(element: postProcessResult.element, diagnostics: processResult.diagnostics + postProcessResult.diagnostics, passContext: postProcessResult.passContext)
  }

  func visit(_ identifier: Identifier, passContext: ASTPassContext) -> ASTPassResult<Identifier> {
    let processResult = pass.process(identifier: identifier, passContext: passContext)
    let postProcessResult = pass.postProcess(identifier: processResult.element, passContext: processResult.passContext)
    return ASTPassResult(element: postProcessResult.element, diagnostics: processResult.diagnostics + postProcessResult.diagnostics, passContext: postProcessResult.passContext)
  }

  func visit(_ type: Type, passContext: ASTPassContext) -> ASTPassResult<Type> {
    var processResult = pass.process(type: type, passContext: passContext)

    processResult.element.genericArguments = processResult.element.genericArguments.map { genericArgument in
      return processResult.combining(visit(genericArgument, passContext: processResult.passContext))
    }

    let postProcessResult = pass.postProcess(type: processResult.element, passContext: passContext)
    return ASTPassResult(element: postProcessResult.element, diagnostics: processResult.diagnostics + postProcessResult.diagnostics, passContext: postProcessResult.passContext)
  }

  func visit(_ callerCapability: CallerCapability, passContext: ASTPassContext) -> ASTPassResult<CallerCapability> {
    var processResult = pass.process(callerCapability: callerCapability, passContext: passContext)

    processResult.element.identifier = processResult.combining(visit(processResult.element.identifier, passContext: processResult.passContext))

    let postProcessResult = pass.postProcess(callerCapability: processResult.element, passContext: processResult.passContext)
    return ASTPassResult(element: postProcessResult.element, diagnostics: processResult.diagnostics + postProcessResult.diagnostics, passContext: postProcessResult.passContext)
  }

  func visit(_ expression: Expression, passContext: ASTPassContext) -> ASTPassResult<Expression> {
    var processResult = pass.process(expression: expression, passContext: passContext)

    switch processResult.element {
    case .binaryExpression(let binaryExpression):
      processResult.element = .binaryExpression(processResult.combining(visit(binaryExpression, passContext: processResult.passContext)))
    case .bracketedExpression(let expression):
      processResult.element = .bracketedExpression(processResult.combining(visit(expression, passContext: processResult.passContext)))
    case .functionCall(let functionCall):
      processResult.element = .functionCall(processResult.combining(visit(functionCall, passContext: processResult.passContext)))
    case .identifier(let identifier):
      processResult.element = .identifier(processResult.combining(visit(identifier, passContext: processResult.passContext)))
    case .literal(_), .self(_): break
    case .variableDeclaration(let variableDeclaration):
      processResult.element = .variableDeclaration(processResult.combining(visit(variableDeclaration, passContext: processResult.passContext)))
    case .subscriptExpression(let subscriptExpression):
      processResult.element = .subscriptExpression(processResult.combining(visit(subscriptExpression, passContext: processResult.passContext)))
    }

    let postProcessResult = pass.postProcess(expression: processResult.element, passContext: processResult.passContext)
    return ASTPassResult(element: postProcessResult.element, diagnostics: processResult.diagnostics + postProcessResult.diagnostics, passContext: postProcessResult.passContext)
  }

  func visit(_ statement: Statement, passContext: ASTPassContext) -> ASTPassResult<Statement> {
    var processResult = pass.process(statement: statement, passContext: passContext)

    switch processResult.element {
    case .expression(let expression):
      processResult.element = .expression(processResult.combining(visit(expression, passContext: processResult.passContext)))
    case .returnStatement(let returnStatement):
      processResult.element = .returnStatement(processResult.combining(visit(returnStatement, passContext: processResult.passContext)))
    case .ifStatement(let ifStatement):
      processResult.element = .ifStatement(processResult.combining(visit(ifStatement, passContext: processResult.passContext)))
    }

    let postProcessResult = pass.postProcess(statement: processResult.element, passContext: processResult.passContext)
    return ASTPassResult(element: postProcessResult.element, diagnostics: processResult.diagnostics + postProcessResult.diagnostics, passContext: postProcessResult.passContext)
  }

  func visit(_ binaryExpression: BinaryExpression, passContext: ASTPassContext) -> ASTPassResult<BinaryExpression> {
    var processResult = pass.process(binaryExpression: binaryExpression, passContext: passContext)

    if case .punctuation(let punctuation) = binaryExpression.op.kind, punctuation.isAssignment  {
      processResult.passContext.asLValue = true
    }
    processResult.element.lhs = processResult.combining(visit(processResult.element.lhs, passContext: processResult.passContext))
    processResult.passContext.asLValue = false

    processResult.element.rhs = processResult.combining(visit(processResult.element.rhs, passContext: processResult.passContext))

    let postProcessResult = pass.postProcess(binaryExpression: processResult.element, passContext: processResult.passContext)
    return ASTPassResult(element: postProcessResult.element, diagnostics: processResult.diagnostics + postProcessResult.diagnostics, passContext: postProcessResult.passContext)
  }

  func visit(_ functionCall: FunctionCall, passContext: ASTPassContext) -> ASTPassResult<FunctionCall> {
    var processResult = pass.process(functionCall: functionCall, passContext: passContext)

    processResult.element.identifier = processResult.combining(visit(processResult.element.identifier, passContext: processResult.passContext))

    processResult.element.arguments = processResult.element.arguments.map { argument in
      return processResult.combining(visit(argument, passContext: processResult.passContext))
    }

    let postProcessResult = pass.postProcess(functionCall: processResult.element, passContext: processResult.passContext)
    return ASTPassResult(element: postProcessResult.element, diagnostics: processResult.diagnostics + postProcessResult.diagnostics, passContext: postProcessResult.passContext)
  }

  func visit(_ subscriptExpression: SubscriptExpression, passContext: ASTPassContext) -> ASTPassResult<SubscriptExpression> {
    var processResult = pass.process(subscriptExpression: subscriptExpression, passContext: passContext)

    processResult.element.baseIdentifier = processResult.combining(visit(processResult.element.baseIdentifier, passContext: processResult.passContext))

    processResult.element.indexExpressions = processResult.element.indexExpressions.map { indexExpression in
      processResult.combining(visit(indexExpression, passContext: processResult.passContext))
    }

    let postProcessResult = pass.postProcess(subscriptExpression: processResult.element, passContext: processResult.passContext)
    return ASTPassResult(element: postProcessResult.element, diagnostics: processResult.diagnostics + postProcessResult.diagnostics, passContext: postProcessResult.passContext)
  }

  func visit(_ returnStatement: ReturnStatement, passContext: ASTPassContext) -> ASTPassResult<ReturnStatement> {
    var processResult = pass.process(returnStatement: returnStatement, passContext: passContext)

    if let expression = processResult.element.expression {
      processResult.element.expression = processResult.combining(visit(expression, passContext: processResult.passContext))
    }

    let postProcessResult = pass.postProcess(returnStatement: processResult.element, passContext: processResult.passContext)
    return ASTPassResult(element: postProcessResult.element, diagnostics: processResult.diagnostics + postProcessResult.diagnostics, passContext: postProcessResult.passContext)
  }

  func visit(_ ifStatement: IfStatement, passContext: ASTPassContext) -> ASTPassResult<IfStatement> {
    var processResult = pass.process(ifStatement: ifStatement, passContext: passContext)

    processResult.element.condition = processResult.combining(visit(processResult.element.condition, passContext: processResult.passContext))

    processResult.element.body = processResult.element.body.map { statement in
      return processResult.combining(visit(statement, passContext: processResult.passContext))
    }

    processResult.element.elseBody = processResult.element.body.map { statement in
      return processResult.combining(visit(statement, passContext: processResult.passContext))
    }

    let postProcessResult = pass.postProcess(ifStatement: processResult.element, passContext: processResult.passContext)
    return ASTPassResult(element: postProcessResult.element, diagnostics: processResult.diagnostics + postProcessResult.diagnostics, passContext: postProcessResult.passContext)
  }
}
