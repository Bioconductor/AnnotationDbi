
This directory contains version 1.0 of the schemas used in the sqlite-based
annotation data packages.

IMPORTANT: Version 1.0 of the schemas is our target for BioC 2.1 release
i.e. it is the version that we plan to use in the data packages that will
be released with Bioconductor 2.1 (AnnotationDbi 1.0.0).
At this time (Aug 20, 2007) the schemas _effectively_ in use to produce the
data packages that we put in BioC 2.1 devel for testing purpose are slightly
different but will converge to version 1.0 before the release.
See extdata/schemas_tmp for the schemas currently in use.

The 1.0 schemas are portable (tested on SQLite and PostgreSQL only so far,
still need to test them on MySQL and Oracle), self-describing, easier to
read and easier to understand than the current schemas.


Summary of modifications from the current schemas
=================================================

All schemas:

  o add DBSCHEMAVERSION entry (with value "1.0") in the "metadata" table
  o renamed "qcdata" table -> "map_counts"
  o explicit use of the NULL or NOT NULL constraint on every column (except
    on primary key cols that are implicitly NOT NULL)
  o added a PRIMARY KEY on map_counts.name and metadata.name
  o replaced the non-standard TEXT type by one of the standard SQL
    character types (see "Table of character types" below)
  o made sure that referenced tables are created before referencing tables
  o use portable FOREIGN KEY syntax (compatible with MySQL+InnoDB and PostgreSQL)
      FOREIGN KEY (col) REFERENCES table (col)

All probe-based and org-based schemas:

  o renamed internal id "id" -> "_id"
  o renamed chromosome_locations.chromosome -> chromosome_locations.seqname
  o renamed kegg.kegg_id col -> kegg.path_id
  o renamed chrlengths.chr col -> chrlengths.chromosome
  o added UNIQUE constraint on genes.gene_id and gene_info._id

All probe-based schemas:

  o added a PRIMARY KEY constraint on probes.probe_id
  o moved "_id" col to last position in "probes" table
  o removed the "accessions" table

GO_DB schema:

  o renamed "term_id" cols -> "_id"
  o renamed "offspring_id" cols -> "_offspring_id"
  o renamed "parent_id" cols -> "_parent_id"
  o renamed "evidence" cols -> "relationship_type"
  o removed "gene2go_evidence" table
  o moved "ontology" col to 1st position in "go_ontology" table
  o added UNIQUE constraint on go_ontology.term_type and go_term.go_id
  o added foreign key on [go_term|go_obsolete].ontology: REFERENCES go_ontology
  o changed go_synonym.like_go_id type from INTEGER to SMALLINT
  o removed go_obsolete.term_id column and made go_obsolete.go_id the
    new PRIMARY KEY

  Note that, ideally, we could add UNIQUE constraints on (_id, _parent_id) in
  the go_[bp|cc|mf]_parents tables and on (_id, _offspring_id) in the
  go_[bp|cc|mf]_offspring tables but the cost of doing this is high (+5M for
  the size of GO.sqlite: 37M instead of 32M).

KEGG_DB schema:
  o added UNIQUE constraint on pathway2name.path_id and pathway2name.path_name


Table of character types
========================

  Real world type                        SQL type       Columns
  ------------------------------------   ------------   ---------
  Entrez Gene ID                         VARCHAR(10)    genes.gene_id
  manufacturer ID                        VARCHAR(80)    probes.probe_id
  GenBank accession number               VARCHAR(20)    probes.accession
                                                        accessions.accession
  gene symbol or alias                   VARCHAR(80)    alias.alias_symbol
                                                        gene_info.symbol
  chromosome name                        VARCHAR(2)     chromosomes.chromosome
                                                        chrlengths.chromosome
  sequence name                          VARCHAR(20)    chromosome_locations.seqname
  cytoband location                      VARCHAR(20)    cytogenetic_locations.cytogenetic_location
  EC number (no "EC:" prefix)            VARCHAR(13)    ec.ec_number
  gene name                              VARCHAR(255)   gene_info.gene_name
  OMIM ID                                CHAR(6)        omim.omim_id
  IPI accession number                   CHAR(11)       pfam.ipi_id
                                                        prosite.ipi_id
  Pfam ID                                CHAR(7)        pfam.pfam_id
  PROSITE ID                             CHAR(7)        prosite.prosite_id
  PubMed ID                              VARCHAR(10)    pubmed.pubmed_id
  RefSeq accession number                VARCHAR(20)    refseq.accession
  UniGene ID                             VARCHAR(10)    unigene.unigene_id

  GO ontology (short label)              VARCHAR(9)     [go_ontology|go_term|go_obsolete].ontology
  GO ontology (full label)               VARCHAR(18)    go_ontology.term_type
  GO ID                                  CHAR(10)       go_id
  textual label for the GO term          VARCHAR(255)   [go_term|go_obsolete].term
  textual definition for the GO term     TEXT           [go_term|go_obsolete].definition
  type of GO child-parent relationship   VARCHAR(7)     go_[bp|cc|mf]_parents.relationship_type
  GO evidence code                       CHAR(3)        go_[bp|cc|mf].evidence

  KEGG pathway short ID                  CHAR(5)        [kegg|pathway2name].path_id
  KEGG pathway name                      VARCHAR(80)    pathway2name.path_name
  EC number (with "EC:" prefix)          VARCHAR(16)    ec2go.ec_no
  KEGG pathway long ID                   CHAR(8)        pathway2gene.pathway_id
  Entrez Gene or ORF ID                  VARCHAR(20)    pathway2gene.gene_id

Notes: - MySQL supports VARCHAR up to 255 chars only.
       - These types should be used consistently across all schemas.

