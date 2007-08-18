
The *_DB.sql files in this directory contain the new DB schemas.

The new schemas are portable (tested on SQLite and PostgreSQL only so far,
still need to test them on MySQL and Oracle), self-describing, easier to
read and easier to understand than the current ones.


Summary of modifications from the current schemas
=================================================

All schemas:
  o renamed "qcdata" table -> "map_counts"
  o explicit use of the NULL or NOT NULL constraint on every column (except
    on primary key cols that are implicitly NOT NULL)
  o added a PRIMARY KEY on map_counts.name and metadata.name
  o replaced the non-standard TEXT type by one of the standard SQL
    character types (see "Table of character types" below)
  o made sure that referenced tables are created before referencing tables
  o removed col specification from foreign key defs, e.g.:
      REFERENCES genes(_id)
    with
      REFERENCES genes

All probe-based and org-based schemas:
  o renamed internal id "id" -> "_id"
  o renamed chrlengths.chr col -> "chromosome"
  o added UNIQUE constraint on genes.gene_id and gene_info._id

All probe-based schemas:
  o added a PRIMARY KEY constraint on probes.probe_id
  o moved "_id" col in last position in "probes" table

GO_DB schema:
  o renamed "term_id" -> "_id"
  o renamed "evidence" cols -> "relationship_type"
  o removed "gene2go_evidence" table
  o moved "ontology" col to 1st position in "go_ontology" table
  o added UNIQUE constraint on go_ontology.term_type and go_term.go_id
  o added foreign key on go_term.ontology: REFERENCES go_ontology


Table of character types
========================

  Description                            Type           Columns
  ------------------------------------   ------------   ---------
  Entrez Gene ID                         VARCHAR(10)    genes.gene_id
  manufacturer ID                        VARCHAR(80)    probes.probe_id
  GenBank accession number               VARCHAR(20)    probes.accession, accessions.accession
  gene symbol or alias                   VARCHAR(80)    alias.alias_symbol, gene_info.symbol
  chromosome name                        VARCHAR(2)     chromosomes.chromosome
                                                        chromosome_locations.chromosome
                                                        chrlengths.chromosome
  gene name                              VARCHAR(255)   gene_info.gene_name
  GO ontology (short version)            VARCHAR(9)     [go_ontology|go_term|go_obsolete].ontology
  GO ontology (full version)             VARCHAR(18)    go_ontology.term_type
  GO term ID                             CHAR(10)       go_[bp|cc|mf].go_id
  GO evidence code                       CHAR(3)        go_[bp|cc|mf].evidence
  Textual label of a GO term             VARCHAR(255)   [go_term|go_obsolete].term
  Textual definition of a GO term        TEXT           [go_term|go_obsolete].definition
  Type of GO child-parent relationship   VARCHAR(7)     go_[bp|cc|mf]_parents.relationship_type
  KEGG ID                           CHAR(5)        kegg.kegg_id
  OMIM ID                           CHAR(6)        omim.omim_id
  IPI accession number              CHAR(11)       pfam.ipi_id, prosite.ipi_id
  Pfam ID                           CHAR(7)        pfam.pfam_id
  PROSITE ID                        CHAR(7)        prosite.prosite_id
  PubMed ID                         VARCHAR(10)    pubmed.pubmed_id
  RefSeq accession number           VARCHAR(20)    refseq.accession
  UniGene ID                        VARCHAR(10)    unigene.unigene_id

Notes: - MySQL supports VARCHAR up to 255 chars only.
       - These types should be used consistently across all schemas.

