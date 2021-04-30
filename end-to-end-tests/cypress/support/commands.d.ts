/// <reference types="cypress" />

declare namespace Cypress {
  interface Chainable<Subject> {
    getByTestId(
      ...args: Parameters<Cypress.Chainable<Subject>["get"]>
    ): Chainable<JQuery<HTMLElement>>;

    /**
     * Creates a log that the rest of the commands are nested under and
     * it will also track progress of the commands in the parent log
     *
     * @example
     * // When consoleProps are not used or known up front
     * cy.withOverallNameLogged({ displayName: "SOME COMMAND" }, () => {
     *   cy.get("something").should("not.exist");
     *   cy.click("button-selector");
     *   cy.get("something").should("exist");
     * });
     *
     * // When consoleProps are needed and not known up front
     * cy.withOverallNameLogged({ displayName: "SOME COMMAND" }, (consolePropsSetter) => {
     *   cy.get("something").should("not.exist");
     *   cy.click("button-selector");
     *   cy.get("something").should("exist").then(element => {
     *     consolePropsSetter({ keyName: element.someProperty })
     *   });
     * });
     */
    withOverallNameLogged<T>(
      logConfig: Partial<LogConfig>,
      commandsCallback: (
        consolePropsSetter: (
          props: ReturnType<LogConfig["consoleProps"]>
        ) => void
      ) => T
    ): T;
  }
}
