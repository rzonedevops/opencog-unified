/**
 * cognitive-visualization.js
 * 
 * Phase II.4: Interactive Cognitive Visualization
 * Real-time visualization with recursive attention feedback
 */

class CognitiveVisualization {
    constructor() {
        this.canvas = document.getElementById('cognitiveCanvas');
        this.ctx = this.canvas.getContext('2d');
        
        // Visualization state
        this.nodes = new Map();
        this.edges = new Map();
        this.agents = new Map();
        this.patterns = new Map();
        this.attentionOverlays = new Map();
        
        // Enhanced ECAN visualization state
        this.ecanState = {
            stiValues: new Map(),
            ltiValues: new Map(),
            economicFlows: new Map(),
            attentionalFocus: [],
            economicPressure: 0.0,
            totalStiFunds: 100000.0
        };
        
        // Pattern matcher visualization state
        this.patternMatchState = {
            activeTraversals: new Map(),
            patternQueries: new Map(),
            matchingProgress: new Map(),
            unificationSteps: []
        };
        
        // Meta-cognitive introspection state
        this.introspectionState = {
            systemMetrics: {},
            cognitiveFlows: new Map(),
            metaCognitiveAwareness: 0.0,
            adaptiveBehavior: false
        };
        
        // Recursive feedback state
        this.feedbackActive = false;
        this.feedbackStrength = 0.0;
        this.cycleCount = 0;
        this.adaptationRate = 0.1;
        
        // Rendering parameters
        this.zoom = 1.0;
        this.panX = 0;
        this.panY = 0;
        this.realTimeMode = false;
        this.updateRate = 30; // Hz
        
        // Attention parameters
        this.attentionThreshold = 0.5;
        this.maxAttentionRadius = 100;
        
        this.initializeCanvas();
        this.setupEventHandlers();
        this.generateSampleData();
        this.startVisualization();
    }

    initializeCanvas() {
        const resizeCanvas = () => {
            const rect = this.canvas.parentElement.getBoundingClientRect();
            this.canvas.width = rect.width;
            this.canvas.height = rect.height;
        };
        
        resizeCanvas();
        window.addEventListener('resize', resizeCanvas);
    }

    setupEventHandlers() {
        // Control panel handlers
        document.getElementById('attentionThreshold').addEventListener('input', (e) => {
            this.attentionThreshold = parseFloat(e.target.value);
        });

        document.getElementById('updateRate').addEventListener('input', (e) => {
            this.updateRate = parseInt(e.target.value);
            document.getElementById('updateRateDisplay').textContent = `${this.updateRate} Hz`;
        });

        document.getElementById('startBtn').addEventListener('click', () => {
            this.startRealTimeMode();
        });

        document.getElementById('pauseBtn').addEventListener('click', () => {
            this.pauseRealTimeMode();
        });

        document.getElementById('resetBtn').addEventListener('click', () => {
            this.reset();
        });

        // Canvas interaction handlers
        this.canvas.addEventListener('click', (e) => {
            this.handleCanvasClick(e);
        });

        this.canvas.addEventListener('mousemove', (e) => {
            this.handleCanvasHover(e);
        });

        // Zoom and pan
        this.canvas.addEventListener('wheel', (e) => {
            e.preventDefault();
            this.handleZoom(e);
        });
    }

    generateSampleData() {
        // Generate sample cognitive nodes
        for (let i = 0; i < 20; i++) {
            const nodeId = `node_${i}`;
            this.nodes.set(nodeId, {
                id: nodeId,
                x: Math.random() * (this.canvas.width - 100) + 50,
                y: Math.random() * (this.canvas.height - 100) + 50,
                state: Math.random(),
                attention: Math.random(),
                type: ['concept', 'predicate', 'evaluation'][Math.floor(Math.random() * 3)]
            });
        }

        // Generate sample edges
        const nodeIds = Array.from(this.nodes.keys());
        for (let i = 0; i < 30; i++) {
            const edgeId = `edge_${i}`;
            const source = nodeIds[Math.floor(Math.random() * nodeIds.length)];
            const target = nodeIds[Math.floor(Math.random() * nodeIds.length)];
            if (source !== target) {
                this.edges.set(edgeId, {
                    id: edgeId,
                    source: source,
                    target: target,
                    weight: Math.random(),
                    type: 'inheritance'
                });
            }
        }

        // Generate sample agents
        for (let i = 0; i < 3; i++) {
            const agentId = `agent_${i}`;
            this.agents.set(agentId, {
                id: agentId,
                x: Math.random() * (this.canvas.width - 100) + 50,
                y: Math.random() * (this.canvas.height - 100) + 50,
                state: [Math.random(), Math.random(), Math.random()],
                active: true,
                frequency: 5 + Math.random() * 10
            });
        }

        this.updateInfoPanels();
    }

    startVisualization() {
        const animate = () => {
            this.update();
            this.render();
            
            if (this.realTimeMode) {
                setTimeout(() => requestAnimationFrame(animate), 1000 / this.updateRate);
            } else {
                requestAnimationFrame(animate);
            }
        };
        
        animate();
    }

    startRealTimeMode() {
        this.realTimeMode = true;
        this.feedbackActive = true;
        document.getElementById('recursiveFeedback').style.display = 'block';
        document.getElementById('statusIndicator').textContent = 'Real-time Mode Active';
    }

    pauseRealTimeMode() {
        this.realTimeMode = false;
        this.feedbackActive = false;
        document.getElementById('recursiveFeedback').style.display = 'none';
        document.getElementById('statusIndicator').textContent = 'Visualization Paused';
    }

    reset() {
        this.nodes.clear();
        this.edges.clear();
        this.agents.clear();
        this.patterns.clear();
        this.attentionOverlays.clear();
        
        this.feedbackStrength = 0.0;
        this.cycleCount = 0;
        
        this.generateSampleData();
        document.getElementById('statusIndicator').textContent = 'Visualization Reset';
    }

    update() {
        if (!this.realTimeMode) return;

        this.cycleCount++;

        // Simulate live AtomSpace data integration
        this.updateFromLiveAtomSpace();
        
        // Update ECAN attention dynamics
        this.updateECANVisualization();
        
        // Update pattern matcher state
        this.updatePatternMatchVisualization();
        
        // Update meta-cognitive introspection
        this.updateIntrospectionState();

        // Simulate cognitive state evolution
        this.nodes.forEach((node) => {
            // Apply attention-driven state changes
            const attentionBoost = this.ecanState.stiValues.get(node.id) || 0.0;
            node.state += (Math.random() - 0.5) * 0.02 + attentionBoost * 0.001;
            node.state = Math.max(0, Math.min(1, node.state));
            
            // Update attention based on state and feedback
            node.attention += (Math.random() - 0.5) * 0.05 + this.feedbackStrength * 0.1;
            node.attention = Math.max(0, Math.min(1, node.attention));
            
            // Store STI values for ECAN visualization
            this.ecanState.stiValues.set(node.id, node.attention * 1000);
        });

        // Simulate agent state evolution with multi-agent coordination
        this.agents.forEach((agent) => {
            agent.state = agent.state.map(s => {
                let newState = s + (Math.random() - 0.5) * 0.03;
                return Math.max(0, Math.min(1, newState));
            });
            
            // Add agent interaction effects
            this.agents.forEach((otherAgent) => {
                if (agent.id !== otherAgent.id) {
                    const distance = this.calculateAgentDistance(agent, otherAgent);
                    if (distance < 100) { // Interaction threshold
                        const influence = 0.01 * (100 - distance) / 100;
                        agent.state = agent.state.map((s, i) => {
                            return Math.max(0, Math.min(1, s + influence * (otherAgent.state[i] - s)));
                        });
                    }
                }
            });
        });

        // Detect emergent patterns with enhanced algorithms
        this.detectEmergentPatterns();

        // Update attention overlays with ECAN integration
        this.updateAttentionOverlays();

        // Apply recursive feedback with meta-cognitive awareness
        if (this.feedbackActive) {
            this.applyRecursiveFeedback();
        }

        this.updateInfoPanels();
    }

    detectEmergentPatterns() {
        // Simple pattern detection based on node clustering
        const clusters = this.findNodeClusters();
        
        clusters.forEach((cluster, index) => {
            if (cluster.length >= 3) {
                const patternId = `pattern_${index}`;
                this.patterns.set(patternId, {
                    id: patternId,
                    nodes: cluster,
                    strength: cluster.length / this.nodes.size,
                    type: 'cluster',
                    detected: Date.now()
                });
            }
        });

        // Remove old patterns
        const now = Date.now();
        this.patterns.forEach((pattern, id) => {
            if (now - pattern.detected > 5000) { // 5 seconds
                this.patterns.delete(id);
            }
        });
    }

    findNodeClusters() {
        const clusters = [];
        const visited = new Set();
        const clusterThreshold = 80; // Distance threshold for clustering

        this.nodes.forEach((node) => {
            if (visited.has(node.id)) return;

            const cluster = [node.id];
            visited.add(node.id);

            this.nodes.forEach((otherNode) => {
                if (visited.has(otherNode.id)) return;

                const distance = Math.sqrt(
                    Math.pow(node.x - otherNode.x, 2) + 
                    Math.pow(node.y - otherNode.y, 2)
                );

                if (distance < clusterThreshold) {
                    cluster.push(otherNode.id);
                    visited.add(otherNode.id);
                }
            });

            if (cluster.length > 1) {
                clusters.push(cluster);
            }
        });

        return clusters;
    }

    updateAttentionOverlays() {
        this.attentionOverlays.clear();

        this.nodes.forEach((node) => {
            if (node.attention > this.attentionThreshold) {
                this.attentionOverlays.set(node.id, {
                    x: node.x,
                    y: node.y,
                    radius: node.attention * this.maxAttentionRadius,
                    intensity: node.attention
                });
            }
        });
    }

    applyRecursiveFeedback() {
        // Calculate average attention level
        let totalAttention = 0;
        this.nodes.forEach(node => totalAttention += node.attention);
        const avgAttention = totalAttention / this.nodes.size;

        // Update feedback strength based on attention distribution
        this.feedbackStrength = avgAttention * this.adaptationRate;

        // Apply feedback to cognitive state
        this.nodes.forEach((node) => {
            // Nodes with high attention influence their neighbors
            if (node.attention > this.attentionThreshold) {
                this.edges.forEach((edge) => {
                    if (edge.source === node.id) {
                        const targetNode = this.nodes.get(edge.target);
                        if (targetNode) {
                            targetNode.attention += this.feedbackStrength * 0.1;
                            targetNode.attention = Math.min(1, targetNode.attention);
                        }
                    }
                });
            }
        });
    }

    render() {
        // Clear canvas
        this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);

        // Apply transforms
        this.ctx.save();
        this.ctx.scale(this.zoom, this.zoom);
        this.ctx.translate(this.panX, this.panY);

        // Render attention overlays (background)
        this.renderAttentionOverlays();

        // Render edges
        this.renderEdges();

        // Render pattern highlights
        this.renderPatterns();

        // Render nodes
        this.renderNodes();

        // Render agents
        this.renderAgents();

        this.ctx.restore();
    }

    renderAttentionOverlays() {
        this.attentionOverlays.forEach((overlay) => {
            const gradient = this.ctx.createRadialGradient(
                overlay.x, overlay.y, 0,
                overlay.x, overlay.y, overlay.radius
            );
            gradient.addColorStop(0, `rgba(0, 255, 65, ${overlay.intensity * 0.3})`);
            gradient.addColorStop(1, 'rgba(0, 255, 65, 0)');

            this.ctx.fillStyle = gradient;
            this.ctx.beginPath();
            this.ctx.arc(overlay.x, overlay.y, overlay.radius, 0, 2 * Math.PI);
            this.ctx.fill();
        });
    }

    renderEdges() {
        this.edges.forEach((edge) => {
            const sourceNode = this.nodes.get(edge.source);
            const targetNode = this.nodes.get(edge.target);
            
            if (sourceNode && targetNode) {
                const alpha = edge.weight * 0.7;
                this.ctx.strokeStyle = `rgba(0, 255, 65, ${alpha})`;
                this.ctx.lineWidth = edge.weight * 3;
                this.ctx.beginPath();
                this.ctx.moveTo(sourceNode.x, sourceNode.y);
                this.ctx.lineTo(targetNode.x, targetNode.y);
                this.ctx.stroke();
            }
        });
    }

    renderNodes() {
        this.nodes.forEach((node) => {
            // Node base
            const radius = 8 + node.state * 12;
            const intensity = node.attention;
            
            this.ctx.fillStyle = `rgba(0, 255, 65, ${0.7 + intensity * 0.3})`;
            this.ctx.beginPath();
            this.ctx.arc(node.x, node.y, radius, 0, 2 * Math.PI);
            this.ctx.fill();

            // Node glow
            if (intensity > this.attentionThreshold) {
                this.ctx.shadowColor = '#00ff41';
                this.ctx.shadowBlur = intensity * 20;
                this.ctx.beginPath();
                this.ctx.arc(node.x, node.y, radius, 0, 2 * Math.PI);
                this.ctx.fill();
                this.ctx.shadowBlur = 0;
            }

            // Node label
            this.ctx.fillStyle = '#000';
            this.ctx.font = '8px monospace';
            this.ctx.textAlign = 'center';
            this.ctx.fillText(node.type[0].toUpperCase(), node.x, node.y + 2);
        });
    }

    renderAgents() {
        this.agents.forEach((agent) => {
            // Agent circle
            this.ctx.strokeStyle = '#ff6b35';
            this.ctx.lineWidth = 3;
            this.ctx.beginPath();
            this.ctx.arc(agent.x, agent.y, 15, 0, 2 * Math.PI);
            this.ctx.stroke();

            // Agent glow
            if (agent.active) {
                this.ctx.shadowColor = '#ff6b35';
                this.ctx.shadowBlur = 15;
                this.ctx.beginPath();
                this.ctx.arc(agent.x, agent.y, 15, 0, 2 * Math.PI);
                this.ctx.stroke();
                this.ctx.shadowBlur = 0;
            }

            // Agent ID
            this.ctx.fillStyle = '#ff6b35';
            this.ctx.font = '10px monospace';
            this.ctx.textAlign = 'center';
            this.ctx.fillText(agent.id.split('_')[1], agent.x, agent.y + 3);
        });
    }

    renderPatterns() {
        this.patterns.forEach((pattern) => {
            if (pattern.nodes.length < 2) return;

            // Calculate bounding box
            let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
            
            pattern.nodes.forEach(nodeId => {
                const node = this.nodes.get(nodeId);
                if (node) {
                    minX = Math.min(minX, node.x - 20);
                    minY = Math.min(minY, node.y - 20);
                    maxX = Math.max(maxX, node.x + 20);
                    maxY = Math.max(maxY, node.y + 20);
                }
            });

            // Draw pattern highlight
            this.ctx.strokeStyle = `rgba(255, 255, 0, ${pattern.strength})`;
            this.ctx.lineWidth = 2;
            this.ctx.setLineDash([5, 5]);
            this.ctx.strokeRect(minX, minY, maxX - minX, maxY - minY);
            this.ctx.setLineDash([]);
        });
    }

    handleCanvasClick(event) {
        const rect = this.canvas.getBoundingClientRect();
        const x = (event.clientX - rect.left) / this.zoom - this.panX;
        const y = (event.clientY - rect.top) / this.zoom - this.panY;

        // Check for node clicks
        this.nodes.forEach((node) => {
            const distance = Math.sqrt(Math.pow(x - node.x, 2) + Math.pow(y - node.y, 2));
            if (distance < 20) {
                this.generateVisualizationFeedback('click', node.id, 1.0);
            }
        });

        // Check for agent clicks
        this.agents.forEach((agent) => {
            const distance = Math.sqrt(Math.pow(x - agent.x, 2) + Math.pow(y - agent.y, 2));
            if (distance < 20) {
                this.generateVisualizationFeedback('click', agent.id, 1.0);
            }
        });
    }

    handleCanvasHover(event) {
        const rect = this.canvas.getBoundingClientRect();
        const x = (event.clientX - rect.left) / this.zoom - this.panX;
        const y = (event.clientY - rect.top) / this.zoom - this.panY;

        // Apply hover feedback with lower intensity
        this.nodes.forEach((node) => {
            const distance = Math.sqrt(Math.pow(x - node.x, 2) + Math.pow(y - node.y, 2));
            if (distance < 30) {
                this.generateVisualizationFeedback('hover', node.id, 0.2);
            }
        });
    }

    handleZoom(event) {
        const zoomFactor = event.deltaY > 0 ? 0.9 : 1.1;
        this.zoom *= zoomFactor;
        this.zoom = Math.max(0.1, Math.min(3.0, this.zoom));
    }

    generateVisualizationFeedback(interactionType, targetId, strength) {
        if (!this.feedbackActive) return;

        // Create feedback signal
        const feedbackSignal = {
            type: interactionType,
            target: targetId,
            strength: strength,
            timestamp: Date.now()
        };

        // Apply feedback to target
        const targetNode = this.nodes.get(targetId);
        const targetAgent = this.agents.get(targetId);

        if (targetNode) {
            targetNode.attention += strength * 0.2;
            targetNode.attention = Math.min(1, targetNode.attention);
        }

        if (targetAgent) {
            targetAgent.state = targetAgent.state.map(s => Math.min(1, s + strength * 0.1));
        }

        // Update feedback strength
        this.feedbackStrength = Math.min(1, this.feedbackStrength + strength * 0.1);

        // Visual feedback
        document.getElementById('recursiveFeedback').style.animation = 'none';
        setTimeout(() => {
            document.getElementById('recursiveFeedback').style.animation = 'feedback-flash 0.5s ease-out';
        }, 10);
    }

    updateInfoPanels() {
        // Update counters
        document.getElementById('nodeCount').textContent = this.nodes.size;
        document.getElementById('edgeCount').textContent = this.edges.size;
        document.getElementById('agentCount').textContent = this.agents.size;
        document.getElementById('patternCount').textContent = this.patterns.size;

        // Update feedback info
        document.getElementById('feedbackStrength').textContent = this.feedbackStrength.toFixed(3);
        document.getElementById('cycleCount').textContent = this.cycleCount;
        document.getElementById('adaptationRate').textContent = this.adaptationRate.toFixed(2);

        // Update attention list with ECAN data
        const attentionList = document.getElementById('attentionList');
        attentionList.innerHTML = '';
        
        const sortedNodes = Array.from(this.nodes.values())
            .sort((a, b) => b.attention - a.attention)
            .slice(0, 5);

        sortedNodes.forEach(node => {
            const item = document.createElement('div');
            item.className = 'info-item';
            const stiValue = this.ecanState.stiValues.get(node.id) || 0;
            item.innerHTML = `${node.id}: ATT=${node.attention.toFixed(2)} STI=${stiValue.toFixed(0)}`;
            attentionList.appendChild(item);
        });

        // Update pattern list with enhanced pattern data
        const patternList = document.getElementById('patternList');
        patternList.innerHTML = '';
        
        this.patterns.forEach(pattern => {
            const item = document.createElement('div');
            item.className = 'info-item';
            const confidence = this.patternMatchState.matchingProgress.get(pattern.id) || 0;
            item.innerHTML = `${pattern.type}: ${pattern.nodes.length} nodes (${pattern.strength.toFixed(2)}) conf=${confidence.toFixed(2)}`;
            patternList.appendChild(item);
        });

        // Update agent status with coordination info
        const agentStatus = document.getElementById('agentStatus');
        agentStatus.innerHTML = '';
        
        this.agents.forEach(agent => {
            const item = document.createElement('div');
            item.className = 'info-item';
            const avgState = agent.state.reduce((a, b) => a + b, 0) / agent.state.length;
            const coordination = this.calculateAgentCoordination(agent);
            item.innerHTML = `${agent.id}: ${avgState.toFixed(2)} @ ${agent.frequency.toFixed(1)}Hz coord=${coordination.toFixed(2)}`;
            agentStatus.appendChild(item);
        });
        
        // Update ECAN economic indicators
        const ecanIndicator = document.createElement('div');
        ecanIndicator.innerHTML = `<strong>ECAN Status:</strong> STI Funds: ${this.ecanState.totalStiFunds.toFixed(0)}, Economic Pressure: ${this.ecanState.economicPressure.toFixed(2)}`;
        
        // Update meta-cognitive awareness
        const metaIndicator = document.createElement('div');
        metaIndicator.innerHTML = `<strong>Meta-Cognitive:</strong> Awareness: ${this.introspectionState.metaCognitiveAwareness.toFixed(2)}, Adaptive: ${this.introspectionState.adaptiveBehavior}`;
    }
}

// Initialize the visualization when the page loads
document.addEventListener('DOMContentLoaded', () => {
    window.cognitiveVisualization = new CognitiveVisualization();
});

// Enhanced Methods for Live System Integration

CognitiveVisualization.prototype.updateFromLiveAtomSpace = function() {
    // Simulate connection to live AtomSpace data
    // In production, this would use WebSocket or HTTP APIs to get real data
    console.log('🔄 Updating from live AtomSpace data...');
    
    // Simulate receiving new nodes and attention values
    if (Math.random() < 0.1) { // 10% chance of new data
        const newNodeId = `live_node_${Date.now()}`;
        this.nodes.set(newNodeId, {
            id: newNodeId,
            x: Math.random() * (this.canvas.width - 100) + 50,
            y: Math.random() * (this.canvas.height - 100) + 50,
            state: Math.random(),
            attention: Math.random(),
            type: 'live_concept'
        });
    }
};

CognitiveVisualization.prototype.updateECANVisualization = function() {
    // Update Economic Attention Networks visualization
    console.log('💰 Updating ECAN visualization...');
    
    // Simulate economic dynamics
    let totalSTI = 0;
    this.nodes.forEach(node => {
        const currentSTI = this.ecanState.stiValues.get(node.id) || 0;
        // Apply economic pressure and wage/rent dynamics
        const economicChange = (Math.random() - 0.5) * 50;
        const newSTI = Math.max(0, currentSTI + economicChange);
        this.ecanState.stiValues.set(node.id, newSTI);
        totalSTI += newSTI;
    });
    
    // Update economic pressure
    this.ecanState.economicPressure = totalSTI / this.ecanState.totalStiFunds;
    
    // Update attentional focus
    this.ecanState.attentionalFocus = Array.from(this.ecanState.stiValues.entries())
        .filter(([id, sti]) => sti > 500)
        .map(([id, sti]) => id);
};

CognitiveVisualization.prototype.updatePatternMatchVisualization = function() {
    // Update pattern matcher state visualization
    console.log('🔍 Updating pattern matcher visualization...');
    
    // Simulate pattern matching progress
    this.patterns.forEach(pattern => {
        const currentProgress = this.patternMatchState.matchingProgress.get(pattern.id) || 0;
        const progressChange = (Math.random() - 0.3) * 0.1; // Bias toward progress
        const newProgress = Math.max(0, Math.min(1, currentProgress + progressChange));
        this.patternMatchState.matchingProgress.set(pattern.id, newProgress);
        
        // Create unification steps
        if (Math.random() < 0.2) { // 20% chance of new unification step
            this.patternMatchState.unificationSteps.push({
                patternId: pattern.id,
                step: `Unifying ${pattern.nodes[0]} with candidate nodes`,
                timestamp: Date.now(),
                success: Math.random() > 0.3
            });
            
            // Keep only recent steps
            if (this.patternMatchState.unificationSteps.length > 10) {
                this.patternMatchState.unificationSteps.shift();
            }
        }
    });
};

CognitiveVisualization.prototype.updateIntrospectionState = function() {
    // Update meta-cognitive introspection state
    console.log('🧠 Updating introspection state...');
    
    // Calculate cognitive coherence
    const coherence = this.calculateCognitiveCoherence();
    
    // Update system metrics
    this.introspectionState.systemMetrics = {
        cognitive_coherence: coherence,
        attention_stability: this.calculateAttentionStability(),
        pattern_formation_rate: this.patterns.size / Math.max(1, this.cycleCount),
        agent_coordination: this.calculateOverallAgentCoordination(),
        meta_cognitive_awareness: this.introspectionState.metaCognitiveAwareness
    };
    
    // Update meta-cognitive awareness based on system state
    const targetAwareness = (coherence + this.introspectionState.systemMetrics.attention_stability) / 2;
    this.introspectionState.metaCognitiveAwareness += 0.1 * (targetAwareness - this.introspectionState.metaCognitiveAwareness);
    
    // Enable adaptive behavior if awareness is high
    this.introspectionState.adaptiveBehavior = this.introspectionState.metaCognitiveAwareness > 0.7;
    
    // Adapt system parameters based on introspection
    if (this.introspectionState.adaptiveBehavior) {
        this.adaptSystemParameters();
    }
};

CognitiveVisualization.prototype.calculateAgentDistance = function(agent1, agent2) {
    return Math.sqrt(
        Math.pow(agent1.x - agent2.x, 2) + 
        Math.pow(agent1.y - agent2.y, 2)
    );
};

CognitiveVisualization.prototype.calculateAgentCoordination = function(agent) {
    let coordination = 0;
    let agentCount = 0;
    
    this.agents.forEach(otherAgent => {
        if (agent.id !== otherAgent.id) {
            const stateSimilarity = agent.state.reduce((sum, state, index) => {
                return sum + (1 - Math.abs(state - otherAgent.state[index]));
            }, 0) / agent.state.length;
            
            coordination += stateSimilarity;
            agentCount++;
        }
    });
    
    return agentCount > 0 ? coordination / agentCount : 0;
};

CognitiveVisualization.prototype.calculateOverallAgentCoordination = function() {
    let totalCoordination = 0;
    this.agents.forEach(agent => {
        totalCoordination += this.calculateAgentCoordination(agent);
    });
    return this.agents.size > 0 ? totalCoordination / this.agents.size : 0;
};

CognitiveVisualization.prototype.calculateCognitiveCoherence = function() {
    // Calculate coherence based on node state similarity and connectivity
    let coherence = 0;
    let connectionCount = 0;
    
    this.edges.forEach(edge => {
        const sourceNode = this.nodes.get(edge.source);
        const targetNode = this.nodes.get(edge.target);
        
        if (sourceNode && targetNode) {
            const stateSimilarity = 1 - Math.abs(sourceNode.state - targetNode.state);
            const attentionSimilarity = 1 - Math.abs(sourceNode.attention - targetNode.attention);
            coherence += (stateSimilarity + attentionSimilarity) / 2;
            connectionCount++;
        }
    });
    
    return connectionCount > 0 ? coherence / connectionCount : 0;
};

CognitiveVisualization.prototype.calculateAttentionStability = function() {
    // Calculate how stable attention allocation is over time
    let stability = 0;
    this.nodes.forEach(node => {
        // Use attention value as proxy for stability (in real system, would track variance)
        stability += node.attention;
    });
    return this.nodes.size > 0 ? stability / this.nodes.size : 0;
};

CognitiveVisualization.prototype.adaptSystemParameters = function() {
    // Adapt system parameters based on meta-cognitive insights
    console.log('🔧 Adapting system parameters based on meta-cognitive analysis...');
    
    const metrics = this.introspectionState.systemMetrics;
    
    // Adapt attention threshold based on coherence
    if (metrics.cognitive_coherence < 0.5) {
        this.attentionThreshold = Math.max(0.1, this.attentionThreshold - 0.05);
    } else if (metrics.cognitive_coherence > 0.8) {
        this.attentionThreshold = Math.min(1.0, this.attentionThreshold + 0.05);
    }
    
    // Adapt feedback strength based on agent coordination
    if (metrics.agent_coordination < 0.3) {
        this.adaptationRate = Math.min(0.5, this.adaptationRate + 0.01);
    } else if (metrics.agent_coordination > 0.7) {
        this.adaptationRate = Math.max(0.05, this.adaptationRate - 0.01);
    }
    
    // Update UI to reflect parameter changes
    const thresholdSlider = document.getElementById('attentionThreshold');
    if (thresholdSlider) {
        thresholdSlider.value = this.attentionThreshold;
    }
    
    const adaptationDisplay = document.getElementById('adaptationRate');
    if (adaptationDisplay) {
        adaptationDisplay.textContent = this.adaptationRate.toFixed(2);
    }
};