
process otu_anno {
    publishDir "output/otus", mode: 'symlink', overwrite: true
    container "registry.cn-hangzhou.aliyuncs.com/wybioinfo/usearch"

    input:
    path otu_table
    path anno
    

    output:
    path "sum_*.tsv"

    script:
    """
    for i in d p c o f g;do
        usearch -sintax_summary ${anno} \
            -otutabin ${otu_table} -rank \${i} \
            -output  sum_\${i}.tsv
    done

    """
}

workflow{
    otu_table = channel.value(params.otu_table.content)
    anno = channel.value(params.otu_anno.anno)

    otu_table.view()
    anno.view()
    otu_anno(otu_table, anno)
}