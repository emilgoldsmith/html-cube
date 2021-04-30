// ***********************************************
// This example commands.js shows you how to
// create various custom commands and overwrite
// existing commands.
//
// For more comprehensive examples of custom
// commands please read more here:
// https://on.cypress.io/custom-commands
// ***********************************************
//
//
// -- This is a parent command --
// Cypress.Commands.add("login", (email, password) => { ... })
//
//
// -- This is a child command --
// Cypress.Commands.add("drag", { prevSubject: 'element'}, (subject, options) => { ... })
//
//
// -- This is a dual command --
// Cypress.Commands.add("dismiss", { prevSubject: 'optional'}, (subject, options) => { ... })
//
//
// -- This will overwrite an existing command --
// Cypress.Commands.overwrite("visit", (originalFn, url, options) => { ... })

const getByTestId: Cypress.Chainable<undefined>["getByTestId"] = (
  testId,
  ...args
) => cy.get(`[data-testid=${testId}]`, ...args);
Cypress.Commands.add("getByTestId", getByTestId);

const withOverallNameLogged: Cypress.Chainable<undefined>["withOverallNameLogged"] = function (
  logConfig,
  commandsCallback
) {
  const log = Cypress.log({
    ...logConfig,
    ...(logConfig.autoEnd === true ? {} : { autoEnd: false }),
  });
  const consolePropsSetter = (consoleProps: Cypress.ObjectLike): void => {
    log.set({ consoleProps: () => consoleProps });
  };
  const handleEndOfCommand = () => {
    log.snapshot("after");
    if (logConfig.autoEnd !== true) {
      log.end();
    }
  };

  log.snapshot("before");
  const callbackReturnValue = commandsCallback(consolePropsSetter);
  if (Cypress.isCy(callbackReturnValue)) {
    return callbackReturnValue.then((returnValue) => {
      handleEndOfCommand();
      return returnValue;
    }) as typeof callbackReturnValue;
  }
  cy.wrap(undefined, { log: false }).then(handleEndOfCommand);
  return callbackReturnValue;
};
Cypress.Commands.add("withOverallNameLogged", withOverallNameLogged);
