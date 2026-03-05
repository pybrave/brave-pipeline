
process otu_table {
    publishDir "output/otus", mode: 'symlink', overwrite: true
    container "registry.cn-hangzhou.aliyuncs.com/wybioinfo/vsearch:v1.28"

    input:
    path otus
    path otutab

    output:
    path "otutab.tsv"

    script:
    """
    
    sed 's/^>\\(.*\\)_\\(.*\\)/>\\1.\\2/' ${otutab} > ${otutab}.rename.fa
    vsearch --usearch_global ${otutab}.rename.fa \
        --db ${otus} \
        --id 0.97 --threads ${task.cpus} \
        --otutabout otutab.tsv
    """
}

workflow{
    ch_input = channel.fromList(params.otutab).map(it->it.fa)
    ch_otus = channel.value(params.otus.fa)
    ch_input.view()
    // print(params.otutab)
    otu_table(ch_otus, ch_input)
}