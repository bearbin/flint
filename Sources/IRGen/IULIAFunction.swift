//
//  IULIAFunction.swift
//  IRGen
//
//  Created by Franklin Schrans on 12/28/17.
//

import AST
import Foundation
import CryptoSwift

struct IULIAFunction {
  static let returnVariableName = "ret"

  var functionDeclaration: FunctionDeclaration
  var contractIdentifier: Identifier
  var capabilityBinding: Identifier?
  var callerCapabilities: [CallerCapability]

  var contractStorage: ContractStorage
  var environment: Environment

  let variablePool = VariablePool()
  let memoryPool = MemoryPool()

  var name: String {
    return functionDeclaration.identifier.name
  }

  var parameterNames: [String] {
    return functionDeclaration.explicitParameters.map({ mangleIdentifierName($0.identifier.name) })
  }

  var parameterCanonicalTypes: [CanonicalType] {
    return functionDeclaration.explicitParameters.map({ CanonicalType(from: $0.type.rawType)! })
  }

  var resultCanonicalType: CanonicalType? {
    return functionDeclaration.resultType.flatMap({ CanonicalType(from: $0.rawType)! })
  }

  func rendered() -> String {
    let doesReturn = functionDeclaration.resultType != nil
    let parametersString = parameterNames.joined(separator: ", ")
    let signature = "\(name)(\(parametersString)) \(doesReturn ? "-> \(IULIAFunction.returnVariableName)" : "")"

    let callerCapabilityChecks = renderCallerCapabilityChecks(callerCapabilities: callerCapabilities)
    let body = renderBody(functionDeclaration.body)

    let capabilityBindingDeclaration: String
    if let capabilityBinding = capabilityBinding {
      capabilityBindingDeclaration = "let \(mangleIdentifierName(capabilityBinding.name)) := caller()\n"
    } else {
      capabilityBindingDeclaration = ""
    }

    let payableValueDeclaration: String
    if let payableValueParameter = functionDeclaration.firstPayableValueParameter {
      payableValueDeclaration = "let \(mangleIdentifierName(payableValueParameter.identifier.name)) := callvalue()\n"
    } else {
      payableValueDeclaration = ""
    }

    return """
    function \(signature) {
      \(callerCapabilityChecks.indented(by: 2))\(payableValueDeclaration.indented(by: 2))\(capabilityBindingDeclaration.indented(by: 2))\(body.indented(by: 2))
    }
    """
  }

  func renderBody<S : RandomAccessCollection & RangeReplaceableCollection>(_ statements: S) -> String where S.Element == AST.Statement, S.Index == Int, S.SubSequence: RandomAccessCollection {
    guard !statements.isEmpty else { return "" }
    var statements = statements
    let first = statements.removeFirst()
    let firstCode = render(first)
    let restCode = renderBody(statements)

    if case .ifStatement(let ifStatement) = first, ifStatement.endsWithReturnStatement {
      let defaultCode = """

      default {
        \(restCode.indented(by: 2))
      }
      """
      return firstCode + (restCode.isEmpty ? "" : defaultCode)
    } else {
      return firstCode + (restCode.isEmpty ? "" : "\n" + restCode)
    }
  }

  func mangledSignature() -> String {
    let name = functionDeclaration.identifier.name
    let parametersString = parameterCanonicalTypes.map({ $0.rawValue }).joined(separator: ",")

    return "\(name)(\(parametersString))"
  }

  func mangleIdentifierName(_ name: String) -> String {
    return "_\(name)"
  }

  func renderCallerCapabilityChecks(callerCapabilities: [CallerCapability]) -> String {
    let checks = callerCapabilities.flatMap { callerCapability in
      guard !callerCapability.isAny else { return nil }

      let type = environment.type(of: callerCapability.identifier, contractIdentifier: contractIdentifier)!
      let offset = contractStorage.offset(for: callerCapability.name)

      switch type {
      case .fixedSizeArrayType(_, let size):
        return (0..<size).map { index in
          "_flintCallerCheck := add(_flintCallerCheck, \(IULIARuntimeFunction.isValidCallerCapability.rawValue)(sload(add(\(offset), \(index)))))"
          }.joined(separator: "\n")
      case .arrayType(_):
        return "_flintCallerCheck := add(_flintCallerCheck, \(IULIARuntimeFunction.isCallerCapabilityInArray.rawValue)(\(offset)))"
      default:
        return "_flintCallerCheck := add(_flintCallerCheck, \(IULIARuntimeFunction.isValidCallerCapability.rawValue)(sload(\(offset))))"
      }
    }

    if !checks.isEmpty {
      return """
      let _flintCallerCheck := 0
      \(checks.joined(separator: "\n"))
      if eq(_flintCallerCheck, 0) { revert(0, 0) }
      """ + "\n"
    }

    return ""
  }
}

extension IULIAFunction {
  func render(_ statement: AST.Statement) -> String {
    switch statement {
    case .expression(let expression): return render(expression, targetVariable: "test")
    case .ifStatement(let ifStatement): return render(ifStatement)
    case .returnStatement(let returnStatement): return render(returnStatement)
    }
  }

  func render(_ expression: Expression, asLValue: Bool = false, targetVariable: String) -> String {
    switch expression {
    case .binaryExpression(let binaryExpression): return render(binaryExpression, asLValue: asLValue, targetVariable: targetVariable)
    case .bracketedExpression(let expression): return render(expression, targetVariable: targetVariable)
    case .functionCall(let functionCall): return render(functionCall, targetVariable: targetVariable)
    case .identifier(let identifier): return render(identifier, asLValue: asLValue, targetVariable: targetVariable)
    case .variableDeclaration(let variableDeclaration): return render(variableDeclaration, targetVariable: targetVariable)
    case .literal(let literal): return render(literalToken: literal, targetVariable: targetVariable)
    case .self(let `self`): return render(selfToken: self, targetVariable: targetVariable)
    case .subscriptExpression(let subscriptExpression): return render(subscriptExpression, asLValue: asLValue, targetVariable: targetVariable)
    }
  }

  func render(_ binaryExpression: BinaryExpression, asLValue: Bool, targetVariable: String) -> String {
    if case .equal = binaryExpression.opToken {
      return renderAssignment(lhs: binaryExpression.lhs, rhs: binaryExpression.rhs, targetVariable: targetVariable)
    }
    if case .dot = binaryExpression.opToken {
      return renderPropertyAccess(lhs: binaryExpression.lhs, rhs: binaryExpression.rhs, asLValue: asLValue, targetVariable: targetVariable)
    }

    let lhsVariable = variablePool.next()
    let lhsComputation = render(binaryExpression.lhs, asLValue: asLValue, targetVariable: lhsVariable)

    let rhsVariable = variablePool.next()
    let rhsComputation = render(binaryExpression.rhs, asLValue: asLValue, targetVariable: rhsVariable)

    let op: String
    
    switch binaryExpression.opToken {
    case .plus: op = "add"
    case .minus: op = "sub"
    case .times: op = "mul"
    case .divide: op = "div"
    case .closeAngledBracket: op = "lt"
    case .lessThanOrEqual: op = "le"
    case .openAngledBracket: op = "gt"
    case .greaterThanOrEqual: op = "ge"
    default: fatalError()
    }

    return """
    \(lhsComputation)
    \(rhsComputation)
    let \(targetVariable) := \(op)(\(lhsVariable), \(rhsVariable))
    """
  }

  func renderAssignment(lhs: Expression, rhs: Expression, targetVariable: String) -> String {
    let rhsVariable = variablePool.next()
    let rhsComputation = render(rhs, targetVariable: rhsVariable)

    switch lhs {
    case .variableDeclaration(let variableDeclaration):
      return """
      \(rhsComputation)
      let \(mangleIdentifierName(variableDeclaration.identifier.name)) := \(rhsVariable)
      """
    case .identifier(let identifier) where !identifier.isPropertyAccess:
      return """
      \(rhsComputation)
      \(mangleIdentifierName(identifier.name)) := \(rhsVariable)
      """
    default:
      let lhsVariable = variablePool.next()
      let lhsComputation = render(lhs, asLValue: true, targetVariable: lhsVariable)

      let rhsVariable = variablePool.next()
      let rhsComputation = render(rhs, targetVariable: rhsVariable)
      return """
      \(lhsComputation)
      \(rhsComputation)
      sstore(\(lhsVariable), \(rhsVariable))
      """
    }
  }

  func renderPropertyAccess(lhs: Expression, rhs: Expression, asLValue: Bool, targetVariable: String) -> String {
    let rhsCode = render(rhs, asLValue: asLValue, targetVariable: targetVariable)

    if case .self(_) = lhs {
      return rhsCode
    }

    fatalError("Not supported yet.")
  }

  func render(_ functionCall: FunctionCall, targetVariable: String) -> String {
    if let eventCall = environment.matchEventCall(functionCall, contractIdentifier: contractIdentifier) {
      let types = eventCall.type.genericArguments

      var stores = [String]()
      var memoryOffset = 0
      for (i, argument) in functionCall.arguments.enumerated() {
        let argumentVariable = variablePool.next()
        stores.append("""
        \(render(argument, targetVariable: argumentVariable))
        mstore(\(memoryOffset), \(argumentVariable))
        """)
        memoryOffset += types[i].rawType.size * 32
      }

      let totalSize = types.reduce(0) { return $0 + $1.rawType.size } * 32
      let typeList = eventCall.type.genericArguments.map { type in
        return "\(CanonicalType(from: type.rawType)!.rawValue)"
      }.joined(separator: ",")

      let eventHash = "\(eventCall.identifier.name)(\(typeList))".sha3(.keccak256)
      let log = "log1(0, \(totalSize), 0x\(eventHash))"

      return """
      \(stores.joined(separator: "\n"))
      \(log)
      """
    }

    let argumentPairs: [(computation: String, variable: String)] = functionCall.arguments.map({ argument in
      let variable = variablePool.next()
      return (render(argument, targetVariable: variable), variable)
    })
    let argumentComputations = argumentPairs.map { $0.computation }.joined(separator: "\n")
    let argumentVariables = argumentPairs.map { $0.variable }.joined(separator: ", ")

    return """
    \(argumentComputations)
    \(functionCall.identifier.name)(\(argumentVariables))
    """
  }

  func render(_ identifier: Identifier, asLValue: Bool = false, targetVariable: String) -> String {
    if identifier.isPropertyAccess {
      let offset = contractStorage.offset(for: identifier.name)
      if asLValue {
        return "let \(targetVariable) := \(offset)"
      }
      return "let \(targetVariable) := sload(\(offset))"
    }
    return "let \(targetVariable) := \(mangleIdentifierName(identifier.name))"
  }

  func render(_ variableDeclaration: VariableDeclaration, targetVariable: String) -> String {
    return "var \(variableDeclaration.identifier)"
  }

  func render(literalToken: Token, targetVariable: String) -> String {
    guard case .literal(let literal) = literalToken.kind else {
      fatalError("Unexpected token \(literalToken.kind).")
    }

    let value: String

    switch literal {
    case .boolean(let boolean): value = boolean == .false ? "0" : "1"
    case .decimal(.real(let num1, let num2)): value = "\(num1).\(num2)"
    case .decimal(.integer(let num)): value = "\(num)"
    case .string(let string): value = "\"\(string)\""
    }

    return "let \(targetVariable) := \(value)"
  }

  func render(selfToken: Token, targetVariable: String) -> String {
    guard case .self = selfToken.kind else {
      fatalError("Unexpected token \(selfToken.kind)")
    }
    return ""
  }

  func render(_ subscriptExpression: SubscriptExpression, asLValue: Bool = false, targetVariable: String) -> String {
    let baseIdentifier = subscriptExpression.baseIdentifier

    let offset = contractStorage.offset(for: baseIdentifier.name)

    let indexVariable = variablePool.next()
    let indexExpressionCode = render(subscriptExpression.indexExpressions[0], targetVariable: indexVariable)

    let type = environment.type(of: subscriptExpression.baseIdentifier, contractIdentifier: contractIdentifier)!

    guard baseIdentifier.isPropertyAccess else {
      fatalError("Subscriptable types are only supported for contract properties right now.")
    }

    switch type {
    case .arrayType(let elementType):
      guard subscriptExpression.indexExpressions.count == 1 else {
        fatalError("Nested arrays are not supported yet.")
      }
      let variable = variablePool.next()
      let storageArrayOffset = """
      \(indexExpressionCode)
      let \(variable) := \(IULIARuntimeFunction.storageArrayOffset.rawValue)(\(offset), \(indexVariable))
      """
      if asLValue {
        return """
        \(storageArrayOffset)
        let \(targetVariable) := \(variable)
        """
      } else {
        guard elementType.size == 1 else {
          fatalError("Loading array elements of size > 1 is not supported yet.")
        }
        return """
        \(storageArrayOffset)
        let \(targetVariable) := sload(\(variable))
        """
      }
    case .fixedSizeArrayType(let elementType, _):
      let variable = variablePool.next()
      let storageArrayOffset = """
      \(indexExpressionCode)
      let \(variable) := \(IULIARuntimeFunction.storageFixedSizeArrayOffset.rawValue)(\(offset), \(indexVariable), \(type.size))
      """
      if asLValue {
        return """
        \(storageArrayOffset)
        let \(targetVariable) := \(variable)
        """
      } else {
        guard elementType.size == 1 else {
          fatalError("Loading array elements of size > 1 is not supported yet.")
        }
        return """
        \(storageArrayOffset)
        let \(targetVariable) := sload(\(variable))
        """
      }
    case .dictionaryType(key: let keyType, value: let valueType):
      var storeOffset = 0

      var memoryStores = ["mstore(\(storeOffset), \(offset))"]
      storeOffset += 32

      for indexExpression in subscriptExpression.indexExpressions {
        let variable = variablePool.next()
        memoryStores.append("""
          \(render(indexExpression, targetVariable: variable))
          mstore(\(storeOffset), \(variable))
          """)
        storeOffset += 32
      }

      guard keyType.size == 1 else {
        fatalError("Dictionary keys of size > 1 are not supported yet.")
      }

//      let storageDictionaryOffsetForKey = "\(IULIARuntimeFunction.storageDictionaryOffsetForKey.rawValue)(\(offset), \(indexExpressionCode))"
      let offset = "sha3(0, \(storeOffset))"

      if asLValue {
        return """
        \(memoryStores.joined(separator: "\n"))
        let \(targetVariable) := \(offset)
        """
      } else {
        guard valueType.size == 1 else {
          fatalError("Loading dictionary values of size > 1 is not supported yet.")
        }
        return """
        \(memoryStores.joined(separator: "\n"))
        let \(targetVariable) := sload(\(offset))
        """
      }
    default: fatalError()
    }
  }

  func render(_ ifStatement: IfStatement) -> String {
    let conditionVariable = variablePool.next()
    let condition = render(ifStatement.condition, targetVariable: conditionVariable)
    let body = ifStatement.body.map { statement in
      render(statement)
    }.joined(separator: "\n")
    let ifCode: String

    if ifStatement.endsWithReturnStatement {
      ifCode = """
      switch \(condition)
      case 1 {
        \(body.indented(by: 2))
      }
      """
    } else {
      ifCode = """
      if \(condition) {
        \(body.indented(by: 2))
      }
      """
    }

    return ifCode
  }

  func render(_ returnStatement: ReturnStatement) -> String {
    guard let expression = returnStatement.expression else {
      return ""
    }

    let variable = variablePool.next()
    return """
    \(render(expression, targetVariable: variable))
    \(IULIAFunction.returnVariableName) := \(variable)
    """
  }
}

class MemoryPool {
  private var currentIndex = 0
  func next() -> Int {
    defer { currentIndex += 1 }
    return currentIndex
  }
}

class VariablePool {
  private var currentIndex = 0
  func next() -> String {
    defer { currentIndex += 1 }
    return "_flintTmp\(currentIndex)"
  }
}
