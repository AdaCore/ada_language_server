# This file contains the build instructions to generate sources for
# lsp_3_17.gpr from LSP metaModel.json using `gen_lsp-run` tool.
# In its turn, part of `gen_lsp-run` sources are generated from
# LSP JSON Schema (metaModel.schema.json) using `gen_json` tool.
# Sources of `gen_json` tool are located in VSS repository.
#

GEN_JSON     = gen_json
GEN_JSON_DIR = ./generated
LSP_GEN_PRJ  = -P ../../gnat/lsp_gen.gpr
DOC          = ../../doc
TEMP_FILE    ?= /tmp/sources.ada
LSP317_DIR   = ../../liblsp_3_17/source/generated
LSP317_PRJ   = -P ../../gnat/lsp_3_17.gpr
LSP_GEN      = ../../.obj/${SUPERPROJECT}/lsp_gen/lsp_gen-run

PHONY = all update_gen_json build generate

all: build generate

# Update `generated/` (from metaModel.schema.json) sources
update_gen_json: ${DOC}/metaModel.schema.json header.adt
	${GEN_JSON} \
	    --root-package LSP_Gen.Entities --enum-package Enum \
	    --header-file header.adt \
	    --holder "#/definitions/ArrayType:element" \
	    --holder "#/definitions/MapType:value" \
	    ${DOC}/metaModel.schema.json > ${TEMP_FILE}
	gnatchop -w ${TEMP_FILE} ${GEN_JSON_DIR}
	gnatpp ${LSP_GEN_PRJ} ${GEN_JSON_DIR}/*.ad[sb]

# Build gen_lsp-run
build:
	gprbuild ${LSP_GEN_PRJ}

generate: build
	${LSP_GEN} ${DOC}/metaModel.json config/3_17.json5 > ${TEMP_FILE}
	sed -i -e '/^use MarkupContent_Or/s/^/--  /' ${TEMP_FILE}
	gnatchop -gnat2022 -w ${TEMP_FILE} ${LSP317_DIR}
	gnatpp ${LSP317_PRJ} ${LSP317_DIR}/*.ad[sb]
