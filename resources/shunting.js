/**
 * A simple implementation of the Shunting Yard Algorithm
 * functioning as a prototype/poc to quickly test the behaviour.
 */



// first define the input
// const input_str = "1 + f a b * 4 + g 3"
// should be read as "(1 + ((f a b) * 4)) + (g 3)"

// more complicated input
// const input_str = "|> a + b * c ! + |> d"

// this one should cause an error - wrong mixing
const input_str = "|> a ?"


// define fixities
// for each infix operator define associativity and precedence
const fixities = {
  '+'   :   { assoc : 'left'  , precedence : 5  , fixity : 'infix'    },
  '*'   :   { assoc : 'left'  , precedence : 6  , fixity : 'infix'    },
  '|>'  :   { assoc : 'left'  , precedence : 7  , fixity : 'prefix'   },
  '!'   :   { assoc : 'right' , precedence : 8  , fixity : 'postfix'  },
  '?'   :   { assoc : 'right' , precedence : 7  , fixity : 'postfix'  },
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
function shunting_yard() {
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
    if (token_type(tok) == "operator") {
      // first we need to empty the application queue if it contains anything
      if (app_qu.length > 0) {
        const app = `(${app_qu.join(' ')})`
        app_qu = []
        // now let's put the application into the output_qu
        output_qu.push(app)
      }
      // now I need to do the actuall work for operators
      const o1 = tok
      let o2 = undefined
      while (o2 = op_stack[op_stack.length - 1], o2
              &&  
              ( fixities[o2].precedence > fixities[o1].precedence
                    ||
                    (fixities[o2].precedence === fixities[o1].precedence)
              )
            ) {
              // I THINK: if O1 and O2 have equal precedence but different associativities -> throw an error
              if (fixities[o1].precedence === fixities[o2].precedence
                  && fixities[o1].assoc !== fixities[o2].assoc) {
                console.error(`I can not mix \`${o1}\` and \`${o2}\` - please add some parentheses.`)
                process.exit(1)
              }

              if (fixities[o2].precedence === fixities[o1].precedence && fixities[o1].assoc === "left") {
                // pop o2 from the operator stack into the output queue
                op_stack.pop()
                output_qu.push(o2)
              }
      }
      // push o1 onto the operator stack
      op_stack.push(o1)
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


  console.log()
  
  console.log('reversed - so in PRE fix?: ')
  console.log(output_qu.reverse().join(' '))

}

// execute the algorithm
shunting_yard()