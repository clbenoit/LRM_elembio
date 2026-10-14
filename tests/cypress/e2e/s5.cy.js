// Spec RNASeq_S5 (amendement) : analyse dans le Manifest Builder, pas d'onglet dedie.
// App servie sur le port 3333 (bash run.sh 3333, APP_DIR = worktree S5).
// La saisie du tableau simule le payload exact envoye par le widget handsontable
// (Shiny.onInputChange sur "import_indexes.table_input") : fiable, independant
// de la geometrie du DOM handsontable.
describe('RNASeq_S5 dans le Manifest Builder', () => {
  const headerLine =
    'Barcode,Control Type,Sample Name (required),Sample ID,Sample Description,DNA/RNA/Fusions,Reference library,Target regions BED file,Hotspot regions BED file'

  // Payload identique a celui du widget (rootElement.id, data = getData(), params)
  // rootElement.id = "app-import_indexes-table_input" (top module id = "app")
  function setInputS5(rows) {
    cy.window().then((win) => {
      win.Shiny.onInputChange('app-import_indexes-table_input', {
        data: rows,
        changes: { event: 'afterChange', changes: null, source: 'test' },
        params: {
          rClass: 'data.frame',
          rColHeaders: [
            'ANALYSIS_TYPE', 'BC_NUMBER', 'SAMPLE_ID', 'PATIENT_ID', 'REPASSE'
          ],
          rRowHeaders: Array.from({ length: rows.length }, (_, i) => i + 1),
          rColClasses: {
            ANALYSIS_TYPE: 'character', BC_NUMBER: 'character',
            SAMPLE_ID: 'character', PATIENT_ID: 'character',
            // checkbox : true = repasse, null/false = non repasse
            REPASSE: 'logical'
          }
        }
      })
    })
  }

  // L'input visible de selectize (le <select id="app-analysis_name"> est masque)
  const SEL = '#app-analysis_name-selectized'

  function selectAnalysis(value) {
    cy.get(SEL).click()
    cy.get(SEL).type('{backspace}') // retire l'item selectionne (mode single)
    cy.get(SEL).type(value + '{enter}')
    cy.wait(400)
  }

  // Basculer vers RNASeq_S5 et attendre que le tableau soit stable : le re-render
  // du schema declenche un "echo" du widget (loadData, lignes vides) qui arrive
  // quelques ms apres ; on l'attend pour que notre payload soit le dernier mot.
  function selectS5() {
    selectAnalysis('RNASeq_S5')
    cy.get('.handsontable thead th').should('contain.text', 'ANALYSIS_TYPE')
    cy.wait(500)
  }

  function csvFromDownload() {
    cy.get('#app-import_indexes-dlManifest').click()
    return cy.get('#app-import_indexes-dlManifest')
      .invoke('attr', 'href')
      .should('include', '/download/')
      .then((href) => {
        return cy.request(href).then((resp) => {
          expect(resp.status).to.equal(200)
          expect(resp.headers['content-disposition']).to.match(
            /Manifest-RNASeq_S5-\d{4}-\d{2}-\d{2}\.csv/
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

  const ROWS_VALID = [
    ['CTL', '22', '26P1470', 'BENVENUTI', null],
    ['PAN', '23', '26P1475', 'GEVAUDAN', null]
  ]

  beforeEach(() => {
    cy.visit('/')
  })

  it('titre LRM_CHUGA + pas d\'onglet S5', () => {
    cy.title().should('include', 'LRM_CHUGA')
    cy.get('.navbar-brand').should('contain.text', 'LRM_CHUGA')
    cy.contains('a.nav-link', 'S5').should('not.exist')
  })

  it('sélection RNASeq_S5 -> colonnes S5 dans le tableau (REPASSE en checkbox)', () => {
    selectAnalysis('RNASeq_S5')
    cy.get('.handsontable thead th').should('contain.text', 'ANALYSIS_TYPE')
    cy.get('.handsontable thead th').should('contain.text', 'BC_NUMBER')
    cy.get('.handsontable thead th').should('contain.text', 'PATIENT_ID')
    cy.get('.handsontable thead th').should('contain.text', 'REPASSE')
    // colonne REPASSE = checkbox (une par cellule)
    cy.get('#app-import_indexes-table_input input[type="checkbox"]')
      .should('have.length.greaterThan', 0)
  })

  it('lignes valides -> preview + CSV exact (nom de fichier Manifest-RNASeq_S5)', () => {
    selectS5()
    setInputS5(ROWS_VALID)
    cy.get('#app-import_indexes-preview_table')
      .should('contain.text', '26P1470 BEN CTL')
    // la preview ne montre que les lignes de donnees (pas les entetes du CSV)
    cy.get('#app-import_indexes-preview_table')
      .should('not.contain.text', 'CSV Version')
    // ni de colonne rownames : 9 colonnes Ion exactement
    cy.get('#app-import_indexes-preview_table thead th')
      .should('have.length', 9)
    csvFromDownload().then((lines) => {
      expect(lines[2]).to.equal('Ion_V2_RBC1_BC22,,26P1470 BEN CTL,,,RNA,,,')
      expect(lines[3]).to.equal('Ion_V2_RBC1_BC23,,26P1475 GEV PAN,,,RNA,,,')
      expect(lines).to.have.length(4)
    })
  })

  it('REPASSE cochee (true) -> appendé au Sample Name', () => {
    selectS5()
    setInputS5([
      ['CTL', '22', '26P1470', 'BENVENUTI', null],
      ['PAN', '23', '26P1475', 'GEVAUDAN', true]
    ])
    csvFromDownload().then((lines) => {
      expect(lines[3]).to.equal(
        'Ion_V2_RBC1_BC23,,26P1475 GEV PAN REPASSE,,,RNA,,,'
      )
    })
  })

  it('type inconnu -> modal d\'erreur + positions bloquées', () => {
    selectS5()
    setInputS5([
      ['CTL', '22', '26P1470', 'BENVENUTI', null],
      ['XYZ', '23', '26P1475', 'GEVAUDAN', null]
    ])
    cy.get('.swal2-container').should('be.visible')
    cy.contains('.swal2-title', 'Entrees RNASeq_S5 invalides')
    cy.contains('.swal2-html-container', 'type d\'analyse inconnu')
    cy.contains('.swal2-html-container', 'Ligne 2')
    cy.get('.swal2-confirm').click()
  })

  it('bascule vers TS65 -> colonnes standards (LANE)', () => {
    selectS5()
    selectAnalysis('TS65')
    cy.get('.handsontable thead th', { timeout: 10000 })
      .should('contain.text', 'LANE')
    cy.get('.handsontable thead th', { timeout: 10000 })
      .should('contain.text', 'WELL_ID')
  })
})
