
process otu_anno {
    publishDir "output/otus_sintax", mode: 'symlink', overwrite: true
    container "registry.cn-hangzhou.aliyuncs.com/wybioinfo/vsearch:v1.28"

    input:
    path ch_input
    path db_16s
    

    output:
    path "otus.sintax.tsv"

    script:
    """
    vsearch --sintax ${ch_input} \
        --db ${db_16s} \
        --sintax_cutoff 0.1 \
        --tabbedout otus.sintax.tsv
    """
}

workflow{
    ch_input = channel.fromList(params.fasta).map(it->it.fa)
    db_16s = channel.value(params.db_16s.path)
    // ch_input.view()
    otu_anno(ch_input, db_16s)
}