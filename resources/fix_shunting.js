// first define the input
// const input_str = "a b +^ |> |> |> c ! d"
// const input_str = "a * |> b + c" // result POSTfix: a b c + |> * aka (a * (|> (b + c)))

// const input_str = "|] |> a" // result POSTfix: a |> |]
// const input_str = "|> |] a" // result POSTfix: a |] |>

// const input_str = "|> a !" // when ! is weaker

// const input_str = "|] |] |> |> a ! ?" // where ? is strongest but ! is weakest --> ? will have to wait after the whole left part

// const input_str = "|> |] a + b" // where it is interesting when:
// >> the |> is the weakest and the |] is the strongest
// >> or the |> and |] are stronger then +

// const input_str = "|> |] a ! + b * c"

const input_str = ""

console.log(input_str)

// define fixities
// for each infix operator define associativity and precedence
const fixities = {
  '+^'  :   { assoc : 'left'  , precedence : 10 , fixity : 'infix'    },
  '+'   :   { assoc : 'left'  , precedence : 6  , fixity : 'infix'    },
  '*'   :   { assoc : 'left'  , precedence : 9  , fixity : 'infix'    },
  '|>'  :   { assoc : 'right' , precedence : 9  , fixity : 'prefix'   },
  '|]'  :   { assoc : 'right' , precedence : 8  , fixity : 'prefix'   },
  '!'   :   { assoc : 'right' , precedence : 7  , fixity : 'postfix'  },
  '?'   :   { assoc : 'right' , precedence : 15  , fixity : 'postfix'  },
}

// define a function to identify type of the token
function token_type (tok) {
  if (tok in fixities) {
    // cheating a little bit, but that's OK
    return "operator"
  }
  else {
    return "value"
  }
}

// now the algorithm
function fix_shunting_yard() {
  // prepare the input
  const input = input_str.split(' ')

    // operator stack
    const op_stack = []

    // output queue
    const output_qu = []
  
    // application queue
    let app_qu = []

  for (const tok of input) {

    if (token_type(tok) == "value") {
      // put it into the application queue
      app_qu.push(tok)
    }
    
  
    if (token_type(tok) === 'operator') {
      // >>> Then I need to check what fixity it has [pre/in/post]
      if (fixities[tok].fixity === 'prefix') {
        // example |> a    or a + |> b
        // >>> PREfix operator |> has the right of way against the +
        // >>> BUT it also has a right of way against other PREfix operators even stronger ones which came before
        // like example |> |] a
        // where |] has the right of way / advantage even thought the |> is stronger
        // so the |> is on the top of the Operator Stack and |] is o1

        // >>> PREfix operator has an implicit advantage against all operators which are already on the Operator Stack
        // >>> But it also has a responsibility to check if some PREfix operator at the end of the input has a higher precedence then itself
        // >>> if that would happen AND the operator stack and app queue are both empty -> error
        // >>> Check if the operator stack and app queue are both empty and the end of the output is the prefix operator with higher precedence
        if (app_qu.length === 0 && op_stack.length === 0) {
          // >>> check the end of the output
          const end = output_qu[output_qu.length - 1]
          if (end && token_type(end) === 'operator' && fixities[end].precedence > fixities[tok].precedence) {
            // >>> report the error
            // <<< Maybe I could do without this error and instead offer another implicit advantage to the cases like
            // |> |] b + c
            // where even thought |> is strongest and |] is the weakest, it is still understood like |> (|] (b + c))
            // and it feels little bit unintuitive, but it can be like that just fine
            console.error(`Conflict between prefix operator '${tok}' and operator '${end}' which has higher precedence.\nThe precedences should go from left to right like from weak to strong.`)
            throw void 0
            // >>> TODO: or later - just print warning but proceed
          }

          // >>> the Operator Stack and App Queue are empty, BUT that's OK
          // >>> this PREfix operator has the right of way
          // >>> current Operator goes on the Operator Stack
          // op_stack.push(tok) // >>> Not doing that here, common code after the IF and ELSE IF
          // does that before the end of PREfix
        }

        // >>> the Operator Stack or App Queue are not empty
        // >>> If the App Queue is not empty I will need to flush it on the Output
        else if (app_qu.length > 0) {
          const app = `(${app_qu.join(' ')})` // >>> Create actual Expression Application structure representation
          app_qu = []
          output_qu.push(app)
        }
        // >>> Now I have a PREfix operator, but something might be on the Operator Stack
        // >>> in case of example |> |] a when current Operator is |]
        // >>> the |> is on the Operator Stack
        // >>> But no matter how high is the precedence of the |>, the |] will have an implicit advantage
        // >>> later, some POSTfix or INfix Operators might fight the last PREfix operator before the value
        // >>> But that is going to be a problem of that future Operator
        // >>> for now I just put the current operator on the Operator Stack
        op_stack.push(tok)

        // END of PREFIX
      }
      else if (fixities[tok].fixity === 'infix') {
        // >>> Something might be on the App Queue
        // >>> I think I should put it on the Output
        if (app_qu.length > 0) {
          const app = `(${app_qu.join(' ')})` // >>> Create actual Expression Application structure representation
          app_qu = []
          output_qu.push(app)
        }

        const o1 = tok
        let o2 = undefined
        while (o2 = op_stack[op_stack.length - 1], o2) {
          // >>> Infix operator also might be in situation like this example a ! + b
          // >>> the ! (POSTfix) has an implicit advantage (any number of postfix operators with any precedences has it)
          // >>> If this is the case, the o1 needs to wait for all o2s from the Operator Stack
          // >>> Then it can go on the Operator Stack
          if (fixities[o2].fixity === 'postfix') {
            // >>> any POSTfix Operator on the LEFT of the current INfix has an implicit right of way
            // >>> it doesn't matter what precedences they are
            // >>> so the o2 will go on the Output
            op_stack.pop()
            output_qu.push(o2)
          }


          // >>> Infix operator migh be attacked by the PREfix operator as in example |> a + b
          // >>> In this case, they need to fight together
          // >>> They compare precedences and if they have equal --> associativity
          // >>> Actually the o1 needs to fight all possible o2 on the Operator Stack
          
          // >>> BUT the attacker doesn't really need to be a PREfix operator
          // >>> example a + b * c or a * b + c
          // >>> the attacker is INfix operator

          // >>> if the attacker would be a POSTfix, that situation is taken care of already in separate case
          else if (fixities[o1].precedence > fixities[o2].precedence) {
            // >>> o1 is stronger like example |] a + b    where |] is weaker than +
            // >>> that means that o1 will go on the Operator Stack
            // >>> And the loop breaks
            break
          }
          else if (fixities[o1].precedence < fixities[o2].precedence) {
            // >>> o2 is stronger like in example |> a + b    where |> is stronger
            // >>> o2 goes on the Output and we keep looping
            op_stack.pop()
            output_qu.push(o2)
          }
          // >>> they have the same precedence, let's check the associativity
          else if (fixities[o1].precedence === fixities[o2].precedence) {
            // >>> If even one of them is non associative or they have different assocs --> error
            if (fixities[o2].assoc === 'none' || fixities[o1].assoc === 'none') {
              console.error(`Mixing two ('${o1}' and '${o2}') operators with the same precedence but at least one of them is non-associative.`)
              throw void 0
            }
  
            if (fixities[o2].assoc !== fixities[o1].assoc) {
              console.error(`Mixing prefix '${o2}' and postfix '${o1}' operators with the same precedence but different associativity.`)
              throw void 0
            }

            // >>> example |> a + b    if LEFT -> (|> a) + b    if RIGHT -> |> (a + b)
            // >>> example a + b + c    if LEFT -> (a + b) + c    if RIGHT -> a + (b + c)
            // >>>
            if (fixities[o1].assoc === 'left') {
              // example |> a + b or a + b + c
              // >>> if LEFT -> (|> a) + b
              // >>> if LEFT -> (a + b) + c
              // >>> put the o2 on the output and continue looping
              op_stack.pop()
              output_qu.push(o2)
            }
            else if (fixities[o1].assoc === 'right') {
              // example |> a + b or a + b + c
              // >>> if RIGHT -> |> (a + b)
              // >>> if RIGHT -> a + (b + c)
              // >>> put the o1 on the Operator Stack and break the loop
              // op_stack.push(o1) // >>> I won't do it now, because I will do that after the WHILE LOOP
              break
            }
          }

        }

        op_stack.push(o1)

        // END of INFIX
      }
      else if (fixities[tok].fixity === 'postfix') {
        // >>> If there's anything in the app_qu -> put it on the output
        // >>> This application is our argument to the POSTfix operator.
        if (app_qu.length > 0) {
          const app = `(${app_qu.join(' ')})` // >>> Create actual Expression Application structure representation
          app_qu = []
          output_qu.push(app)
        }

        const o1 = tok
        let o2 = undefined
        while (o2 = op_stack[op_stack.length - 1], o2) {
          // console.log('POST loop', fixities[o2], fixities[o1])
          // example a ! ?
          // >>> It doesn't matter how strong the ? is, the only way it does make sense is this (a !) ?
          // >>> That also implies that even if they have the same precedence the associativity doesn't matter
          if (fixities[o2].fixity === 'postfix') {
            // >>> Now the question is - do I put the o2 on the output OR do I put o1 on the op_stack
            // >>> If I put the o1 (?) on the Operator Stack, I am saying that o1(?) has a precedence against o2(!)
            // >>> which it does not.
            // >>> So instead I put the o2 on the Output and keep looping

            op_stack.pop()
            output_qu.push(o2)
          }

          // >>> There might be an operator on the top of the Operator Stack, which is PREfix with higher precedence then this one
          // example |> a !    where |> is stronger then ! ; o1 = ! ; o2 = |>
          else if (fixities[o2].fixity === 'prefix' && fixities[o2].precedence > fixities[o1].precedence) {
            // >>> The O2 will have the value first
            op_stack.pop()
            output_qu.push(o2)
          }
          // >>> Or the o1 has the higher precedence
          // then its |> (a !) and the loop breaks
          else if (fixities[o2].fixity === 'prefix' && fixities[o2].precedence < fixities[o1].precedence) {
            // op_stack.push(o1) // >>> I won't do it now, because I will do that after the WHILE LOOP
            break
          }
          // >>> OR they might have the same precedence, but both have the same associativity and it's LEFT - then the one on the Stack has a right of way
          // example |> a !    where both have the same precedence
          // interesting example |] |> |> a ! ?    where the ? is the strongest, but the ! is weakest ==> ? will have to wait until the very end, because it can't go through !
          // that also means you can do stuff like a b c d _ !    where the _ is some very weak postfix operator and the ! is some very strong operator
          // if we had strong operators (stronger then native function application) that _ would protect the application from the ! ripping the last member from it.
          else if (fixities[o2].fixity === 'prefix' && fixities[o2].precedence === fixities[o1].precedence) {
            // >>> They must have the same associativity
            // >>> If they are LEFT assoc. then the o2 wins
            // >>> otherwise o1 wins
  
            if (fixities[o2].assoc === 'none' || fixities[o1].assoc === 'none') {
              console.error(`Mixing two ('${o1}' and '${o2}') operators with the same precedence but at least one of them is non-associative.`)
              throw void 0
            }
  
            if (fixities[o2].assoc !== fixities[o1].assoc) {
              console.error(`Mixing prefix '${o2}' and postfix '${o1}' operators with the same precedence but different associativity.`)
              throw void 0
            }
  
            // >>> They now have the same associativity which is not 'none'
            // >>> If the assoc is LEFT -> o2 wins
            if (fixities[o2].assoc === 'left') {
              op_stack.pop()
              output_qu.push(o2)
              // >>> But we can keep looping, there might be others who could go ahead of o1 on the Operator Stack
            }
            else {
              // >>> the o1 wins
              // >>> That also means, the loop ends
              // op_stack.push(o1) // >>> I won't do it now, because I will do that after the WHILE LOOP
              break
            }
          }
          
          // >>> Or the top of the Operator Stack is INfix --> the INfix is invading the POSTfix and they must compare their precedences
          // example a + b !    where + is INfix and ! is POSTfix
          else if (fixities[o2].fixity === 'infix' && fixities[o2].precedence > fixities[o1].precedence) {
            // >>> o2 wins
            // (a + b) !
            op_stack.pop()
            output_qu.push(o2)
  
            // >>> potentionaly another previously pushed operator might win
            // example |> a + b !    where ! is weakest and + is strongest
            // then I need to keep looping so that |> can win against !
  
            // or even example a + b + c !    where ! is weakest
            // it again has to lose multiple times
          }
          // >>> Or the o1 is stronger
          else if (fixities[o2].fixity === 'infix' && fixities[o2].precedence < fixities[o1].precedence) {
            // >>> o1 wins and the loop breaks
            // op_stack.push(o1) // >>> I won't do it now, because I will do that after the WHILE LOOP
            break
          }
          // >>> If the precedences are same --> their associativities must be the same and depending on which one it is, one of them will have the right of way.
          // example a + b !    where + is INfix and ! is POSTfix, then it depends on their associativity (must be the same)
          else if (fixities[o2].fixity === 'infix' && fixities[o2].precedence === fixities[o1].precedence) {
            // >>> Test if one of them is none-associative
            if (fixities[o1].assoc === 'none' || fixities[o2].assoc === 'none') {
              console.error(`Mixing infix '${o2}' and postfix '${o1}' operators but at least one of them is none associative.`)
              throw void 0
            }
  
            // >>> Test if they have incorectly different associativities
            if (fixities[o1].assoc !== fixities[o2].assoc) {
              console.error(`Mixing '${o2}' and '${o1}' with the same precedence but with different associativity.`)
              throw void 0
            }
  
            // >>> Now they are associative and have the same assoc
            // >>> if it's LEFT -> o2 wins and we keep looping
            // >>> if it's RIGHT -> o1 wins and loop breaks
  
            if (fixities[o1].assoc === 'left') {
              // >>> o2 wins
              op_stack.pop()
              output_qu.push(o2)
            }
            else if (fixities[o1].assoc === 'right') {
              // >>> o1 wins, the loop breaks
              // op_stack.push(o1) // >>> I won't do it now, because I will do that after the WHILE LOOP
              break
            }
          }
        }

        op_stack.push(o1)

        // END of POSTFIX
      }
      // There is no other option.
    }
  }

  // now flush everything onto the output
  // we are gonna pop operators, so better flush the app qu too
  if (app_qu.length > 0) {
    const app = `(${app_qu.join(' ')})`
    app_qu = []
    // now let's put the application into the output_qu
    output_qu.push(app)
  }

  while (op_stack.length) {
    output_qu.push(op_stack.pop())
  }

  // print the result
  console.log('in POST fix:')
  console.log(output_qu.join(' '))

  console.log('reversed - so in PRE fix?: ')
  console.log(output_qu.reverse().join(' '))
}




// execute the algorithm
fix_shunting_yard()