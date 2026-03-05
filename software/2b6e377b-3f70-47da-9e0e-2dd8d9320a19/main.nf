process merge_fq {
    publishDir "output/merge_fa", mode: 'symlink', overwrite: true
    input:
    path fq_files

    output:
    path 'all.combine.fa'

    """
     gzip -cd  ${fq_files} > all.combine.fa
    """
}

workflow{
    ch_input = channel.fromList(params.fasta).map(it->it.fa)
    // ch_input.collect().view()
    merge_fq(ch_input.collect())

}