process feature_counts {
    tag "$meta.id"
    container "registry.cn-hangzhou.aliyuncs.com/wybioinfo/subread"
    publishDir "output/gene_count/${meta.id}", mode: 'symlink', overwrite: true

    input:
    tuple val(meta),path(bam)
    path(gff)

    output:
    tuple val(meta),path("*.count")
    tuple val(meta),path("*.summary")

    script:
    """
    featureCounts -T ${task.cpus} -p  \
        -a  ${gff}  \
        -F GFF \
        -o ${meta.id}.count \
        -t gene  \
        -g ID  ${bam} 

    """

    stub:
    """
    
    """
}

workflow{
    ch_input = channel.fromList(params.bam).map(it->[[id:it.sample_name],it.bam])
    gff  = channel.value(params.gff.gff)
    feature_counts(ch_input, gff)
}