process prodigal {
    tag "$meta.id"
    publishDir "output/prodigal", mode: 'symlink', overwrite: true
    container "registry.cn-hangzhou.aliyuncs.com/wybioinfo/prodigal:2.6.3"

    input:
    tuple val(meta), path(fasta)

    output:
    tuple val(meta),
          path("${meta.id}.genes.fna"),
          path("${meta.id}.proteins.faa"),
          path("${meta.id}.gff")

    script:
    """
    prodigal -i ${fasta} \
             -o ${meta.id}.gff \
             -a ${meta.id}.proteins.faa \
             -d ${meta.id}.genes.fna \
             -p single
    """
}

workflow{
    ch_input = channel.fromList(params.fasta).map(it->[[id:it.file_name],it.fa])
    ch_input.view()
    prodigal(ch_input)

}