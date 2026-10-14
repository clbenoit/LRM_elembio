// Spec saisonnière : app lancée avec LRM_SEASON=noel sur le port 3333.
// Harnais : package.json / node_modules du change S5 (tests/ du worktree S5),
// lancement avec --spec pointant vers cette spec.
describe('seasonal ui (LRM_SEASON=noel)', () => {
  beforeEach(() => {
    cy.visit('/')
  })

  it('titre de la navbar avec l\'emoji de saison', () => {
    cy.contains('🎄 LRM_CHUGA').should('exist')
  })

  it('bandeau de saison (body::after)', () => {
    cy.get('body').then(($b) => {
      const content = getComputedStyle($b[0], '::after').content
      expect(content).to.include('Joyeuses fêtes')
    })
  })

  it('couche d\'animation (flocons, cadeaux, peres Noel) presente dans le DOM', () => {
    cy.get('.lrm-fx span').should('have.length.gte', 5)
    cy.get('.lrm-fx')
      .should('contain.text', '❄️')
      .and('contain.text', '🎁')
      .and('contain.text', '🎅')
      .and('not.contain.text', '🌨️')
  })

  it('animations masquées après 10 secondes (timer de page)', () => {
    cy.get('.lrm-fx span').should('have.length.gte', 5)
    cy.get('.lrm-fx').should('have.css', 'display', 'block')
    cy.wait(10500)
    cy.get('.lrm-fx').should('have.css', 'display', 'none')
  })

  it('navbar recolorée (fond de saison)', () => {
    cy.get('.navbar').first().then(($n) => {
      expect(getComputedStyle($n[0]).backgroundColor).to.equal('rgb(20, 83, 45)') // #14532d
    })
  })
})
