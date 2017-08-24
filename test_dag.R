digraph boxes_and_circles {
  # a 'graph' statement
  graph [overlap = true, fontsize = 10]
  # several 'node' statements
  node [shape = circle,
        fontname = Helvetica, style = filled, colour = gray, width = 0.6]
  X [x = 2, y = 1]
  Yr [label = <Y<SUB>R</SUB>>]
  W1 [label = <W<SUB>1</SUB>>, x = 0, y = 1]
  W2 [label = <W<SUB>2</SUB>>, x = 1, y = 1]
  node [shape = circle,
        fontname = Helvetica,
        style = dotted, width = 0.6]
  Uy [label = <U<SUB>Y</SUB>>]
  Ux [label = <U<SUB>X</SUB>>]
  # subgraph{rank = same; Yr; X}

  # edge
  X->Yr  Ux->{X, W1, W2} Uy->Yr

}
