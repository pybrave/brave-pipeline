
process beta_div {
    publishDir "output/beta_div", mode: 'symlink', overwrite: true
    container "registry.cn-hangzhou.aliyuncs.com/wybioinfo/usearch"

    input:
    path fasta
    path otutab

    output:
    path "beta_div/*"

    script:
    """
    usearch -cluster_agg ${fasta} -treeout otus.tree
    usearch -beta_div ${otutab} -tree otus.tree \
        -filename_prefix beta_div

    """
}

workflow{
    // ch_input = channel.fromList(params.fasta).map(it->it.fa)
    fasta = channel.value(params.fasta.fa)
    otutab = channel.value(params.otu_table.content)
    // ch_input.collect().view()
    beta_div(fasta, otutab)
}