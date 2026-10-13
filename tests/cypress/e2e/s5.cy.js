describe('S5 tab', () => {
  const validInput =
    'CTL 22 26P1470 BENVENUTI{enter}PAN 23 26P1475 GEVAUDAN'

  const headerLine =
    'Barcode,Control Type,Sample Name (required),Sample ID,Sample Description,DNA/RNA/Fusions,Reference library,Target regions BED file,Hotspot regions BED file'

  function openS5() {
    cy.visit('/')
    cy.contains('a.nav-link', 'S5').click()
    cy.get('#app-s5-s5_input').clear()
  }

  function csvFromDownload() {
    cy.get('#app-s5-dlS5').click()
    return cy.get('#app-s5-dlS5')
      .invoke('attr', 'href')
      .should('include', '/download/')
      .then((href) => {
        return cy.request(href).then((resp) => {
          expect(resp.status).to.equal(200)
          expect(resp.headers['content-disposition']).to.match(
            /S5-\d{4}-\d{2}-\d{2}\.csv/
          )
          const raw =
            typeof resp.body === 'string' ? resp.body : resp.body.toString()
          const lines = raw.trim().split('\n')
          expect(lines[0]).to.equal('CSV Version (required),2.3')
          expect(lines[1]).to.equal(headerLine)
          return lines
        })
      })
  }

  beforeEach(openS5)

  it('valid input -> preview + exact CSV download (no REPASSE)', () => {
    cy.get('#app-s5-s5_input').type(validInput)
    cy.get('#app-s5-load').click()

    cy.get('#app-s5-s5_table .htCore').should('be.visible')
    cy.get('#app-s5-s5_table .htCore td')
      .eq(0)
      .should('contain.text', 'CTL')
    cy.get('#app-s5-s5_table .htCore td')
      .eq(1)
      .should('contain.text', '22')
    cy.get('#app-s5-s5_table .htCore td')
      .eq(2)
      .should('contain.text', '26P1470')
    cy.get('#app-s5-s5_table .htCore td')
      .eq(3)
      .should('contain.text', 'BENVENUTI')

    cy.get('#app-s5-s5_table input[type="checkbox"]').should('have.length', 2)
    cy.get('#app-s5-s5_table input[type="checkbox"]')
      .eq(0)
      .should('not.be.checked')
    cy.get('#app-s5-s5_table input[type="checkbox"]')
      .eq(1)
      .should('not.be.checked')

    csvFromDownload().then((lines) => {
      expect(lines[2]).to.equal('Ion_V2_RBC1_BC22,,26P1470 BEN CTL,,,RNA,,,')
      expect(lines[3]).to.equal('Ion_V2_RBC1_BC23,,26P1475 GEV PAN,,,RNA,,,')
      expect(lines).to.have.length(4)
    })
  })

  it('REPASSE checked -> appended to Sample Name', () => {
    cy.get('#app-s5-s5_input').type(validInput)
    cy.get('#app-s5-load').click()
    cy.get('#app-s5-s5_table input[type="checkbox"]')
      .eq(1)
      .check()
    cy.wait(300)

    csvFromDownload().then((lines) => {
      expect(lines[2]).to.equal('Ion_V2_RBC1_BC22,,26P1470 BEN CTL,,,RNA,,,')
      expect(lines[3]).to.equal(
        'Ion_V2_RBC1_BC23,,26P1475 GEV PAN REPASSE,,,RNA,,,'
      )
    })
  })

  it('unknown CTRL -> error modal + download blocked', () => {
    cy.get('#app-s5-s5_input').type('XYZ 22 26P1470 BENVENUTI')
    cy.get('#app-s5-load').click()

    cy.get('.swal2-container').should('be.visible')
    cy.contains('.swal2-title', 'Input S5 invalide')
    cy.contains('.swal2-html-container', 'type de controle inconnu')
    cy.contains('.swal2-html-container', 'Ligne 1')
    cy.get('.swal2-confirm').click()

    // download bloque : la requete download echoue (4xx/5xx)
    cy.get('#app-s5-dlS5').invoke('attr', 'href').then((href) => {
      if (href && href.includes('/download/')) {
        cy.request({ url: href, failOnStatusCode: false }).then((resp) => {
          expect(resp.status).to.be.within(400, 599)
        })
      }
    })
  })

  it('malformed line (3 fields) -> error modal with line number', () => {
    cy.get('#app-s5-s5_input').type('CTL 22 26P1470')
    cy.get('#app-s5-load').click()

    cy.get('.swal2-container').should('be.visible')
    cy.contains('.swal2-html-container', 'Ligne 1')
    cy.contains('.swal2-html-container', '4 champs')
    cy.get('.swal2-confirm').click()
  })

  it('duplicate BC -> error modal', () => {
    cy.get('#app-s5-s5_input').type('CTL 22 26P1470 BENVENUTI{enter}PAN 22 26P1471 GEVAUDAN')
    cy.get('#app-s5-load').click()

    cy.get('.swal2-container').should('be.visible')
    cy.contains('.swal2-html-container', 'BC duplique')
    cy.contains('.swal2-html-container', '22')
    cy.get('.swal2-confirm').click()
  })
})
